import path from 'node:path';
import fs from 'node:fs/promises';
import { BasicState, IReadonlyState, asyncDependentState, effectNow } from 'statec';
import { Loader } from 'bun';
import { OnLoadResultSourceCode } from 'bun';

type BuildState = {
	htmlTemplate: string,
	bundles: string[],
	files: string[],
};

const allEntrypoints = new BasicState<string[]>([]);
const currentState: IReadonlyState<BuildState> =
	await asyncDependentState(allEntrypoints, async (entrypoints) => {
		console.log("rebuilding", entrypoints);
		if (entrypoints.length == 0) {
			return {
				htmlTemplate: '',
				bundles: [],
				files: [],
			};
		}

		const otherFiles = new Set<string>();

		const build = await Bun.build({
			entrypoints,
			target: 'browser',
			sourcemap: 'external',
			outdir: `./dist`,
			plugins: [
				{
					name: "watch-intercept",
					async setup(build) {
						const { readFileSync } = await import("fs");
						build.onLoad({ filter: /^.*\.(ts|js)$/ }, (args) => {
							const relPath = path.relative(import.meta.dir, args.path);
							console.log(relPath);
							if (relPath.startsWith('src/')) otherFiles.add(relPath);
							const contents = readFileSync(args.path).toString();
							return {
								contents,
								loader: path.extname(args.path).slice(1) as ("js" | "ts"),
							};
						});
					},
				},
			]
		});

		for (const log of build.logs) {
			console.error(log);
		}

		console.log("build done! success?", build.success);
		
		const bundles: string[] = [];
		
		for (const artifact of build.outputs) {
			if (artifact.kind === 'entry-point') {
				bundles.push(path.relative(
					process.cwd(),
					artifact.path
				));
			}
		}
		const htmlTemplate = `
		<!doctype html>
		<html>
		<head>
			<meta charset="utf-8" />
			<link rel="stylesheet" href="/css/normalize.css">
		</head>
		<body>
		${bundles.map(entryPath => 
			`<script src="${entryPathToWebPath(entryPath)}"></script>`
		)}
		</body>
		</html>
		`.trim();
	
		return { htmlTemplate, bundles, files: [...otherFiles.values()] };
	});

const entryPathToWebPath = (entryPath: string) =>
	`/js/${path.basename(entryPath)}`;

let routes: {
	[url: string]: (req: Request) =>
		Response | Promise<Response>
} = {};

currentState.effect((state) => {
	routes = {
		"/": () => {
			return new Response(state.htmlTemplate, {
				headers: { 'Content-Type': 'text/html; charset=utf-8' }
			});
		},
		"/css/normalize.css": async () => {
			const buffer = await fs.readFile('./src/normalize.css');
			return new Response(buffer, {
				headers: { 'Content-Type': 'text/css; charset=utf-8' }
			});
		}
	};

	for (const entryPath of state.bundles) {
		routes[entryPathToWebPath(entryPath)] =
			async () => {
				const buffer = await fs.readFile(entryPath);
				return new Response(buffer, {
					headers: { 'Content-Type': 'application/javascript; charset=utf-8' }
				});
			};
		routes[entryPathToWebPath(entryPath) + '.map'] =
			async () => {
				const buffer = await fs.readFile(entryPath + '.map');
				return new Response(buffer, {
					headers: { 'Content-Type': 'application/json; charset=utf-8' }
				});
			};
	}	
});

allEntrypoints.update(['./src/index.ts']);

const server = Bun.serve({
	port: 3000,
	fetch(req) {
		const url = new URL(req.url);
		if (req.method === "GET" && Object.hasOwn(routes, url.pathname)) {
			return routes[url.pathname](req);
		}
		return new Response(`404! "${url.pathname} not found."`);
	},
});

const setDiff = <T>(a: Set<T>, b: Set<T>) =>
	new Set([...a].filter(x => !b.has(x)));

type Watcher = {
	path: string,
	ac: AbortController,
	watcher: ReturnType<typeof fs.watch>
};

const createWatcher = (path: string) => {
	const ac = new AbortController();
	const w: Watcher = {
		path, ac,
		watcher: fs.watch(path, { signal: ac.signal })
	};
	console.log('created watcher: ', w.path, w.watcher);

	(async () => {
		for await (const { eventType, filename } of w.watcher) {
			console.log('watcher event: ', eventType);
			if (eventType === 'change') {
				allEntrypoints.update(allEntrypoints.get());
			}
		}
	})();

	return w;
};

const watchers: Watcher[] = [];

effectNow(currentState, ({ files }, oldState) => {
	const oldFiles = new Set(oldState?.files ?? []);
	const newFiles = new Set(files);
	
	const removedFiles = setDiff(oldFiles, newFiles);
	const addedFiles = setDiff(newFiles, oldFiles);

	console.log("addedFiles:", addedFiles);
	console.log("removedFiles:", removedFiles);
	
	let toBeRemoved: Watcher[] = [];

	watchers.filter(w => removedFiles.has(w.path)).forEach(w => {
		w.ac.abort();
		toBeRemoved.push(w);
	});

	toBeRemoved.forEach(w => watchers.splice(watchers.indexOf(w), 1));

	addedFiles.forEach(path => {
		watchers.push(createWatcher(path));
	});
});
