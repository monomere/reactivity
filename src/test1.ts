import { BasicState, effectNow, joinedState } from "statec";
import { E, setter } from "./html";

type Either<L, R> = { left: L } | { right: R };
const Left = <L, R>(left: L) => ({ left });
const Right = <L, R>(right: R) => ({ right });

const letIn = <T, U>(x: T, f: (x: T) => U) => f(x);

const logged = <T>(x: T, message?: string) => (console.log(x, ...(message ? [message] : [])), x);

const matchEither = <L, R, Lr, Rr>(
	e: Either<L, R>,
	right: (right: R) => Rr,
	left: (left: L) => Lr
) => 'left' in e ? left(e.left) : right(e.right);

const tail = <R extends any[]>([_, ...tail]: [any, ...R]) => tail;
const head = <T>([head, ..._]: [T, ...any[]]) => head;
const first = head;
const second = <T>([__, second, ..._]: [any, T, ...any[]]) => second;

document.head.appendChild(E("style", el => {
	el.innerHTML = `
input.code {
	border: none;
	background: transparent;
	font-family: monospace;
	padding: 5px;
	width: 100%;
}
button.small {
	font-family: monospace;
	font-size: 10pt;
}

.input-wrap {
	display: flex;
	justify-content: stretch;
}
input.code:focus { outline: none; }
pre.result {
	margin: 1rem;
	padding: 1rem;
	background: #ddd;
	border-left: 5px solid #bbb;
	white-space: pre-wrap;
	font-size: 10pt;
}
pre.result.error {
	background: #fbd;
	border-left: 5px solid #eac;
}
`;
}));

type TokenType =
	| 'name' // [a-zA-Z0-9_]+
	| 'forall' // 'forall'
	| 'let' // 'let'
	| 'in' // 'in'
	| 'dot' // '.'
	| 'lambda' // '\'
	| 'colon' // ':'
	| 'arrow' // '->'
	| 'equals' // '='
	| 'open-paren' | 'close-paren' // '(' ')'
	| 'open-brace' | 'close-brace' // '{' '}'
	;
type Token = { type: TokenType, value?: string };

const tokenToString = (t: Token) =>
t === undefined ? `undefined` :
({
	'name': `${t.value}`,
	'forall': `∀`,
	'let': `let`,
	'in': `in`,
	'dot': `.`,
	'lambda': `λ`,
	'colon': `:`,
	'arrow': `→`,
	'equals': `=`,
	'open-paren': `(`,
	'close-paren': `)`,
	'open-brace': `{`,
	'close-brace': `}`,
})[t.type];

// const tokenToString = (t: Token) =>
// 	t === undefined ? `Token(undefined)` :
// 	`Token(${t.type}${t.value ? `, ${t.value}` : ''})`;

const tokenEqual = (a: Token, b: Token) =>
	a.type === b.type && a.value === b.value;

const tokenize = (source: string): Either<string, Token[]> => {
	const types: [RegExp, TokenType?, number?][] = [
		[/^\s+/],
		[/^in/, 'in'],
		[/^let/, 'let'],
		[/^(forall|[Λ∀])/, 'forall'],
		[/^[\w_]+/, 'name', 0],
		[/^\./, 'dot'],
		[/^[\\λ]/, 'lambda'],
		[/^:/, 'colon'],
		[/^=/, 'equals'],
		[/^(->|[→])/, 'arrow'],
		[/^\(/, 'open-paren'], [/^\)/, 'close-paren'],
		[/^\{/, 'open-brace'], [/^\}/, 'close-brace'],
	];
	const tokens: Token[] = [];
	while (source.length) {
		let added = false;
		for (const [regexp, type, group] of types) {
			const m = source.match(regexp);
			if (m) {
				source = source.slice(m[0].length);
				if (type)
					tokens.push({
						type,
						value: group !== undefined ? m[group] : undefined
					});
				added = true;
				break;
			}
		}
		if (!added) return { left: `unknown character: '${source[0]}'` }
	}
	return { right: tokens };
};

type Typename = string;
type Varname = string;

type TypeAst =
	| { $: 'forall', name: Typename, body: TypeAst }
	| { $: 'arrow', lhs: TypeAst, rhs: TypeAst }
	| { $: 'apply', lhs: TypeAst, rhs: TypeAst }
	| { $: 'name', name: Typename }

type Ast =
	| { $: 'forall', name: Typename, body: Ast }
	| { $: 'lambda', name: Varname, type: TypeAst, body: Ast }
	| { $: 'apply', lhs: Ast, rhs: Ast }
	| { $: 'inst', expr: Ast, type: TypeAst }
	| { $: 'name', name: Varname }

const caseof = <T, U>(v: T, f: (e: T) => U) => f(v);

type ParseResult<T> = Either<[string, boolean?], [T, Token[]]>;
type ParseFunc<T> = (input: Token[]) => ParseResult<T>;

type Parser<T> = {
	seq<U>(p: Parser<U>, asFirst?: boolean): Parser<[T, U]>,
	seql<U>(p: () => Parser<U>, asFirst?: boolean): Parser<[T, U]>,
	to<U>(f: (t: T) => U): Parser<U>,
	(toks: Token[]): ParseResult<T>,
};

const parser = <T>(f: ParseFunc<T>): Parser<T> => {
	const func = ((toks: Token[]) => f(toks)) as Parser<T>;

	func.seql = (p, asFirst = false) => parser((input) =>
		matchEither(func(input),
			([res1, rest]) => matchEither(p()(rest),
				([res2, rest]) => Right([[res1, res2], rest]),
				([message]) => Left([message, !asFirst])),
			Left)
		);

	func.seq = (p, asFirst) => func.seql(() => p, asFirst);

	func.to = (f) => parser((input) =>
		matchEither(func(input),
			([res, rest]) => Right([f(res), rest]),
			Left));

	return func;
};

const zeroOrMore = <T>(p: Parser<T>, allNotFirst = false): Parser<T[]> =>
	parser((input: Token[]) =>
		matchEither(p(input),
			([result, rest]) => matchEither(zeroOrMore(p, true)(rest),
				([results, rest]) => Right([[result, ...results], rest]),
				([message, notFirst]) => notFirst
					? Left([message, true])
					: Right([[], input])
			),
			([message, notFirst]) => (
				// (message === `expected Token(close-paren), got Token(colon).` ?
				// (() => { throw new Error("BAM!"); })() : false),
				notFirst)
					? Left([message, true])
					: Right([[], input])
		)
	);

const oneOrMore = <T>(p: Parser<T>): Parser<T[]> => parser((input: Token[]) =>
	matchEither(p(input),
		([result, rest]) => matchEither(zeroOrMore(p, true)(rest),
			([results, rest]) => Right([[result, ...results], rest]),
			([message, notFirst]) =>
				notFirst
				? Left([message, true])
				: Right([[result], rest])
		),
		Left
	)
);

const eitherOf = <T>(p: Parser<T>, ...ps: Parser<T>[]): Parser<T> =>
	parser((input: Token[]) =>
		matchEither(p(input),
			Right,
			([message, notFirst]) => ps.length && !notFirst
				? eitherOf(...(ps as [Parser<T>, ...Parser<T>[]]))(input)
				: Left([message, notFirst])
		)
	);

const fail = <T>(message: (input: Token[]) => string) =>
	parser<T>((input) => Left([message(input)]));

const satisfy = (pred: (t: Token) => boolean, expected?: string) =>
	parser((input) =>
		input.length
		? caseof(input, ([head, ...rest]) =>
			pred(head)
			? Right([head, rest])
			: Left(expected
					? [`expected ${expected}, got ${tokenToString(head)}.`]
					: [`unexpected ${tokenToString(head)}.`]
				))
		: Left(expected
			? [`expected ${expected}, got end-of-input.`]
			: [`unexpected end-of-input.`]
		)
	);

const parse = (toks: Token[]) => {
	const isToken = (t: Token) => satisfy(t2 => tokenEqual(t, t2), tokenToString(t));
	const name = () => satisfy(t2 => t2.type === 'name', "name").to(t => t.value!);

	const atom = (): Parser<Ast> => eitherOf(
		isToken({ type: 'open-paren' })
			.seql(expression).to(second)
			.seq(isToken({ type: 'close-paren' })).to(first),
		forall(),
		lambda(),
		name().to((name) => ({ $: 'name', name }) as const),
		fail<Ast>(([t]) => `expected an atom, got ${tokenToString(t)} instead.`)
	)
		.seq(zeroOrMore(
			isToken({ type: 'open-brace' })
			.seql(type).to(second)
			.seq(isToken({ type: 'close-brace' })).to(first)
		))
		.to(([expr, types]) =>
			types.reduce<Ast>(
				(expr, type) => ({ $: 'inst', expr, type }),
				expr
			)
		);

	const forallType = () =>
		isToken({ type: 'forall' })
		.seq(oneOrMore(name())).to(second)
		.seq(isToken({ type: 'dot' })).to(([a]) => a.reverse())
		.seql(type)
		.to(([[name, ...names], body]) =>
			names.reduce<TypeAst>(
				(body, name) => ({ $: 'forall', name, body }),
				{ $: 'forall', name, body }
			)
		);

	const typeAtom = (): Parser<TypeAst> =>
		eitherOf(
			forallType(),
			isToken({ type: 'open-paren' })
				.seql(type).to(second)
				.seq(isToken({ type: 'close-paren' })).to(first),
			name().to((name) => ({ $: 'name', name })),
			fail<TypeAst>(([t]) => `expected a type, got ${tokenToString(t)} instead.`));
			
	const typeApp = (): Parser<TypeAst> =>
		oneOrMore(typeAtom()).to(([x, ...xs]) =>
			xs.reduce<TypeAst>((lhs, rhs) => ({ $: 'apply', lhs, rhs }), x));
		
	const type = (): Parser<TypeAst> =>
		typeApp()
		.seq(zeroOrMore(isToken({ type: 'arrow' }).seql(typeAtom).to(second)))
		.to(([type, types]) => letIn([type, ...types].reverse(), ([type, ...types]) =>
			types.reduce<TypeAst>(
				(rhs, lhs) => ({ $: 'arrow', lhs, rhs }),
				type
			)
		));

	const letexpr = (): Parser<Ast> =>
		isToken({ type: 'let' })
		.seql(name).to(second)
		.seq(isToken({ type: 'colon' })).to(first)
		.seql(type)
		.seq(isToken({ type: 'equals' })).to(first)
		.seql(expression)
		.seq(isToken({ type: 'in' })).to(first)
		.seql(expression)
		.to(([[[name, type], value], body]) => ({
			$: 'apply',
			lhs: { $: 'lambda', name, type, body },
			rhs: value
		}));


	const expression = () => eitherOf(
		letexpr(),
		oneOrMore(atom()).to(([x, ...xs]) =>
			xs.reduce<Ast>((lhs, rhs) => ({ $: 'apply', lhs, rhs }), x))
	);

	const declaration = () =>
		isToken({ type: 'open-paren' })
		.seq(name()).to(second)
		.seq(isToken({ type: 'colon' })).to(first)
		.seql(type)
		.seq(isToken({ type: 'close-paren' })).to(first)
		.to(([name, type]) => ({ name, type }))

	const lambda = () =>
		isToken({ type: 'lambda' })
		.seq(oneOrMore(declaration()).to((a) => a.reverse())).to(second)
		.seq(isToken({ type: 'dot' })).to(first)
		.seql(expression)
		.to(([[decl, ...decls], body]) =>
			decls.reduce<Ast>(
				(body, decl) => ({ $: 'lambda', ...decl, body }),
				{ $: 'lambda', ...decl, body }
			)
		);

	const forall = () =>
		isToken({ type: 'forall' })
		.seq(oneOrMore(name()).to((a) => a.reverse())).to(second)
		.seq(isToken({ type: 'dot' })).to(first)
		.seql(expression)
		.to(([[name, ...names], body]) =>
			names.reduce<Ast>(
				(body, name) => ({ $: 'forall', name, body }),
				{ $: 'forall', name, body }
			)
		);

	return expression()(toks);
};

const exhaustedError = (v: any) => {
	throw new Error(`match$ exhausted by ${JSON.stringify(v)}`);
};

const typecheckError = (message: string) => {
	throw new Error(`typecheck error: ${message}`);
}

const evalError = (message: string) => {
	throw new Error(`eval error: ${message}`);
}

type EvalType =
	| { $: 'arrow', lhs: EvalType, rhs: EvalType }
	| { $: 'literal', name: Typename }
	| { $: 'generic', name: Typename, body: EvalType }

type EvalVal =
	| { $: 'lambda', name: Varname, body: Ast }
	| { $: 'literal', name: Varname }

type TypecheckCtx = { types: Map<Typename, EvalType>, vals: Map<Varname, EvalType> };
type EvalCtx = { vals: Map<Varname, EvalVal> };

const tcCtxWithType = (ctx: TypecheckCtx, name: Typename, type: EvalType) => {
	const nctx: TypecheckCtx = { vals: ctx.vals, types: new Map(ctx.types) };
	nctx.types.set(name, type);
	return nctx;
};

const tcCtxWithValType = (ctx: TypecheckCtx, name: Varname, val: EvalType) => {
	const nctx: TypecheckCtx = { vals: new Map(ctx.vals), types: ctx.types };
	nctx.vals.set(name, val);
	return nctx;
};

const evCtxWithVal = (ctx: EvalCtx, name: Varname, val: EvalVal) => {
	const nctx: EvalCtx = { vals: new Map(ctx.vals) };
	nctx.vals.set(name, val);
	return nctx;
};

type Match$<T extends { $: string }, U> = { [V in T as V['$']]: (v: V) => U };
const match$ = <K extends string, T extends { $: K }, U>(
	value: T,
	matches: Partial<Match$<T, U>>,
	otherwise?: (v: T) => U
) => matches[value.$]
	? ((matches as unknown as Match$<T, U>)[value.$] as (v: T) => U)(value)
	: (otherwise ?? exhaustedError)(value);

type Match$pair<T extends { $: string }, U> = { [V in T as V['$']]: (v: [V, V]) => U };
const match$pair = <K extends string, T extends { $: K }, U>(
	value: [T, T],
	key: K,
	matches: Partial<Match$pair<T, U>>,
	otherwise?: (v: [T, T]) => U
) => matches[key]
	? ((matches as unknown as Match$pair<T, U>)[key] as (v: [T, T]) => U)(value)
	: (otherwise ?? exhaustedError)(value);

const typeBetaReduce = (n: EvalType, repl: Typename, v: EvalType): EvalType => match$(n, {
	arrow: ({ lhs, rhs }) => <EvalType>{
		$: 'arrow',
		lhs: typeBetaReduce(lhs, repl, v),
		rhs: typeBetaReduce(rhs, repl, v),
	},
	generic: ({ name, body }) => name === repl
	? <EvalType>{ $: 'generic', name, body }
	: <EvalType>{ $: 'generic', name, body: typeBetaReduce(body, repl, v) },
	literal: ({ name }) => name === repl ? v : <EvalType>{ $: 'literal', name }
});

const typeAstEval = (ctx: TypecheckCtx, n: TypeAst): EvalType => match$(n, {
	apply: ({ lhs, rhs }) => match$(typeAstEval(ctx, lhs), {
		generic: ({ name, body }) => typeBetaReduce(body, name, typeAstEval(ctx, rhs))
	}, (v) => typecheckError(`type ${typeToString(v)} is not a generic type.`) as EvalType),
	arrow: ({ lhs, rhs }) => <EvalType>{
		$: 'arrow',
		lhs: typeAstEval(ctx, lhs),
		rhs: typeAstEval(ctx, rhs)
	},
	forall: ({ name, body }) => <EvalType>{
		$: 'generic',
		name,
		body: typeAstEval(tcCtxWithType(ctx, name, { $: 'literal', name }), body)
	},
	name: ({ name }) => ctx.types.get(name) ?? (name.startsWith('_')
		? <EvalType>{ $: 'literal', name: name.toUpperCase() }
		: typecheckError(`no such type '${name}'`)
	),
});

const typeEqual = (a: EvalType, b: EvalType): boolean =>
	a.$ === b.$ && match$pair([a, b], a.$, {
		arrow: ([{ lhs: lhsa, rhs: rhsa }, { lhs: lhsb, rhs: rhsb }]) =>
			typeEqual(lhsa, lhsb) && typeEqual(rhsa, rhsb),
		generic: ([{ body: bodya }, { body: bodyb }]) => typeEqual(bodya, bodyb),
		literal: ([{ name: namea }, { name: nameb }]) => namea === nameb
	});

const wrapParens = ([s, prec]: [any, number], prec2: number) =>
	prec < prec2 ? `(${s})` : `${s}`;

const typeToString = (t: EvalType, prec: number = 0): string => wrapParens(match$(t, {
	arrow: ({ lhs, rhs }) => [`${typeToString(lhs, 2)} → ${typeToString(rhs, 1)}`, 1],
	generic: ({ name, body }) => [`∀${name}. ${typeToString(body, 0)}`, 0],
	literal: ({ name }) => [`${name}`, Infinity],
}), prec);

const astTypecheck = (ctx: TypecheckCtx, n: Ast): EvalType => match$(n, {
	forall: ({ name, body }) => <EvalType>{
		$: 'generic',
		name, 
		body: astTypecheck(tcCtxWithType(ctx, name, { $: 'literal', name }), body)
	},
	lambda: ({ name, type, body }) => <EvalType>{
		$: 'arrow',
		lhs: typeAstEval(ctx, type),
		rhs: astTypecheck(tcCtxWithValType(ctx, name, typeAstEval(ctx, type)), body)
	},
	apply: ({ lhs, rhs }) => match$(astTypecheck(ctx, lhs), {
		arrow: ({ lhs: lhst, rhs: rhst }) => letIn(astTypecheck(ctx, rhs), argt =>
			typeEqual(argt, lhst)
			? rhst
			: typecheckError(`${typeToString(argt)} is incompatible with ${typeToString(lhst)}.`
			)
		)
	}, (v) => typecheckError(`value ${typeToString(v)} is not an arrow type.`) as EvalType),
	inst: ({ expr, type }) => match$(astTypecheck(ctx, expr), {
		generic: ({ name, body }) => typeBetaReduce(body, name, typeAstEval(ctx, type))
	}, (v) => typecheckError(
		`cannot instantiate ${typeToString(v)}, it's not generic.`) as EvalType
	),
	name: ({ name }) => name.startsWith('_')
		? <EvalType>{ $: 'literal', name: name.toUpperCase() }
		: ctx.vals.get(name) ?? typecheckError(`no such binding '${name}'`),
});

const astEval = (ctx: EvalCtx, n: Ast): EvalVal => match$(n, {
	forall: ({ body }) => astEval(ctx, body),
	lambda: ({ name, body }) => <EvalVal>{ $: 'lambda', name, body },
	apply: ({ lhs, rhs }) => match$(astEval(ctx, lhs), {
		lambda: ({ name, body }) => astEval(evCtxWithVal(ctx, name, astEval(ctx, rhs)), body)
	}, (v) => evalError(`value ${JSON.stringify(v)} is not a lambda.`) as EvalVal),
	inst: ({ expr }) => astEval(ctx, expr),
	name: ({ name }) => ctx.vals.get(name) ?? (name.startsWith('_')
		? <EvalVal>{ $: 'literal', name: name.toLowerCase() }
		: evalError(`no such variable '${name}'`)
	),
});

// const eitherMayThrow = <F extends (...args: any[]) => Either<[string, ...any], any>>(
// 	f: F, ...args: Parameters<F>
// ): ReturnType<F> => {
// 	try {
// 		return f(...args) as ReturnType<F>;
// 	} catch (e: any) {
// 		let message = e.toString();
// 		if (e instanceof Error) {
// 			message = e.message;
// 			console.error(e);
// 			// return Left([`${e.name}: ${e.message}\n${e.stack ?? ''}`]);
// 		}
// 		return Left([message]) as ReturnType<F>;
// 	}
// }

const throwToEither = <F extends (...args: any[]) => any>(
	f: F, ...args: Parameters<F>
): Either<string, ReturnType<F>> => {
	try {
		return Right(f(...args));
	} catch (e: any) {
		let message = e.toString();
		if (e instanceof Error) {
			message = e.message;
			console.error(e);
			// return Left(`${e.name}: ${e.message}\n${e.stack ?? ''}`);
		}
		return Left(message);
	}
}

type LanguageShowType = 'toks' | 'ast' | 'types' | 'eval';
const language = (source: string, show: null | LanguageShowType) => {
	if (show === null) return null;

	source = source.trim();
	if (source.length === 0) return null;

	const toks = tokenize(source);
	if ('left' in toks) return toks;
	if (show === 'toks') return Right(toks.right.map(tokenToString).join(' '));

	const ast = (parse(toks.right));
	if ('left' in ast) return Left(ast.left[0]);

	const indentString = (indent: number) =>
			Array.from({ length: indent + 1 }).join("|   ");

	const stringify = <T extends { $: string }>(o: T, indent: number = 0) => {
		if (o === undefined) return `undefined\n`;
		if (o === null) return `null\n`;
		let s = `${indentString(indent)}- ${o.$}:\n`;
		for (const [key, value] of Object.entries(o)) {
			if (key === '$') continue;
			s += `${indentString(indent)}| ${key}: `;
			if (typeof value === 'string') s += `"${value}"\n`;
			else if (typeof value === 'number') s += `${value}\n`;
			else s += (value ? "\n" : "") + stringify(value, indent + 1);
		}
		return s;
	};

	if (ast.right[1].length > 0) return Left(`excess tokens at the end of input!`);
	if (show === 'ast') return Right(`${stringify(ast.right[0])}`);

	const tcCtx: TypecheckCtx = { vals: new Map(), types: new Map() };
	const typeRes = throwToEither(astTypecheck, tcCtx, ast.right[0]);
	if ('left' in typeRes) return typeRes;
	if (show === 'types') return Right(`${typeToString(typeRes.right)}`);

	const evCtx: EvalCtx = { vals: new Map() };
	const res = throwToEither(astEval, evCtx, ast.right[0]);
	if ('left' in res) return res;
	if (show === 'eval') return Right(`${stringify(res.right)}`);
	return Right(`${stringify(res.right)}`);
};

document.body.append(E("main", el => {
	const examples = [
		`(∀a. λ(x: a). x) {_T} _t`,
		`∀a b. λ(f: (a → a) → a → b) (x: a). f id{a} x`,
		`(λ(id: ∀a. a → a). ∀a b. λ(f: (a → a) → a → b) (x: a). f id{a} x)(∀a. λ(x: a). x)`,
		// `(forall a b. \(f:forall a. a->b)(x:a). f{a} x){_A}{_B}(forall a.\(x:a)._b)_a`,
		// `((λ(id: ∀a. a → a). ∀a b. λ(f: (a → a) → a → b) (x: a). f id{a} x)(∀a. λ(x: a). x)){_A}{_B}`
		// + ` : ((_A → _A) → _A → _B) → _A → _B`,
		// `(λ(f: _A → _A) (x: _A). _b) : (_A → _A) → _A → _B`,
		// `(((λ(id: ∀a. a → a). ∀a b. λ(f: (a → a) → a → b) (x: a). f id{a} x)(∀a. λ(x: a). x)){_A}{_B})(\(x:x).x x)`,
		// `x((x:x).x x)`,
		// `(((λ(id: ∀a. a → a). ∀a b. λ(f: (a → a) → a → b) (x: a). f id{a} x )(∀a. λ(x: a). x)){_A}{_B})( (x:x).x x)`,
		`((λ(id: ∀a. a → a). ∀a b. λ(f: (a → a) → a → b) (x: a). f id{a} x)(∀a. λ(x: a). x)) {_A} {_B}`
		+ ` (λ(f: _A → _A) (x: _A). _b) _a`,
		`let id: ∀a. a → a = ∀a. λ(x: a). x in (∀a b. λ(f: (a → a) → a → b) (x: a). f id{a} x) {_A} {_B}`
		+ ` (λ(f: _A → _A) (x: _A). _b) _a`
	];
	const inputState = new BasicState(examples[examples.length - 1]);
	const resultState = new BasicState<Either<string, string> | null>(null);
	const typeState = new BasicState<LanguageShowType | null>('types');

	effectNow(
		joinedState(inputState, typeState),
		([value, type]) => resultState.update(language(value, type))
	);

	const inputElement: HTMLInputElement = E("input", el => {
		el.placeholder = "Code";
		el.classList.add("code");
		effectNow(inputState, setter(el, 'value'));
		el.oninput = () => inputState.update(el.value);
	});

	el.append(
		E("div", el => {
			el.classList.add("input-wrap");
			el.append(
				...["λ", "∀", "→"].map(char => E("button", el => {
					el.classList.add("small");
					el.textContent = char;
					el.onclick = () => {
						inputState.update(inputState.get() + char);
						inputElement.focus();
					}
				})),
				inputElement,
				E("select", el => {
					const possibleValues = {
						'': "None",
						'toks': "Tokens",
						'ast': "Ast",
						'types': "Types",
						'eval': "Eval",
					};
					el.append(...Object.entries(possibleValues).map(([key, value]) =>
						E("option", el => {
							el.value = key;
							el.textContent = value;
							effectNow(typeState, type => {
								if (key === type) el.selected = true;
							});
						})
					));
					el.oninput = () => {
						typeState.update(
							el.value.length ? el.value as LanguageShowType : null
						);
					}
				}),
			);
		}),
		E("pre", el => {
			effectNow(resultState, (result) => {
				if (result !== null) {
					el.classList.add("result");
					matchEither(result, (right) => {
						el.classList.remove("error");
						el.textContent = right;
					}, (left) => {
						el.classList.add("error");
						el.textContent = left;
					});
				} else {
					el.classList.remove("error");
					el.classList.remove("result");
					el.textContent = "";
				}
			});
		}),
		E("h4", el => { el.textContent = "Test Cases:"; }),
		E("ul", el => {
			for (const example of examples) {
				el.append(E("li", el => {
					el.classList.add("input-wrap");
					el.append(
						E("button", el => {
							el.textContent = "^";
							el.onclick = () => {
								inputState.update(example);
							}
						}),
						E("input", el => {
							el.classList.add("code");
							// el.disabled = true;
							el.readOnly = true;
							el.value = example;
						})
					);
				}));
			}
		}),
	);
}));
