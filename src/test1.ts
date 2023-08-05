import { BasicState, effectNow } from "statec";
import { E, setter } from "./html";

type Either<L, R> = { left: L } | { right: R };
const Left = <L, R>(left: L) => ({ left });
const Right = <L, R>(right: R) => ({ right });

const matchEither = <L, R, Lr, Rr>(
	e: Either<L, R>,
	right: (right: R) => Rr,
	left: (left: L) => Lr
) => 'left' in e ? left(e.left) : right(e.right);

const tail = <R extends any[]>([_, ...tail]: [any, ...R]) => tail;
const head = <T>([head, ..._]: [T, ...any[]]) => head;

document.head.appendChild(E("style", el => {
	el.innerHTML = `
input.code {
	border: none;
	background: transparent;
	font-family: monospace;
	padding: 5px;
	width: 100%;
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
	| 'dot' // '.'
	| 'lambda' // '\'
	| 'colon' // ':'
	| 'arrow' // '->'
	| 'open-paren' | 'close-paren' // '(' ')'
	| 'open-brace' | 'close-brace' // '{' '}'
	;
type Token = { type: TokenType, value?: string };

const tokenToString = (t: Token) =>
	`Token(${t.type}${t.value ? `, ${t.value}` : ''})`;

const tokenEqual = (a: Token, b: Token) =>
	a.type === b.type && a.value === b.value;

const tokenize = (source: string): Either<string, Token[]> => {
	const types: [RegExp, TokenType?, number?][] = [
		[/^\s+/],
		[/^forall/, 'forall'],
		[/^[a-zA-Z0-9_]+/, 'name', 0],
		[/^\./, 'dot'],
		[/^\\/, 'lambda'],
		[/^:/, 'colon'],
		[/^->/, 'arrow'],
		[/^\(/, 'open-paren'], [/^\)/, 'close-paren'],
		[/^\{/, 'open-brace'], [/^\}/, 'close-brace'],
	];
	const tokens: Token[] = [];
	let i = 0, iter = 0;
	while (i < source.length) {
		if (iter++ > 100) break;
		let added = false;
		for (const [regexp, type, group] of types) {
			const m = source.slice(i).match(regexp);
			if (m !== null) {
				i += m[0].length;
				if (type)
					tokens.push({
						type,
						value: group !== undefined ? m[group] : undefined
					});
				added = true;
				break;
			}
		}
		if (!added) return { left: `unknown character: '${source[i]}'` }
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
	| { $: 'name', name: Varname }

const caseof = <T, U>(v: T, f: (e: T) => U) => f(v);

type ParseResult<T> = Either<string[], [T, Token[]]>;
type ParseFunc<T> = (input: Token[]) => ParseResult<T>;

type Parser<T> = {
	seq<U>(p: Parser<U>): Parser<[T, U]>,
	to<U>(f: (t: T) => U): Parser<U>,
	(toks: Token[]): ParseResult<T>,
};

const parser = <T>(f: ParseFunc<T>): Parser<T> => {
	const func = ((toks: Token[]) => f(toks)) as Parser<T>;
	func.seq = (p) => parser((input) =>
		matchEither(func(input),
			([res1, rest]) => matchEither(p(rest),
				([res2, rest]) => Right([[res1, res2], rest]),
				Left),
			Left)
		);

	func.to = (f) => parser((input) =>
		matchEither(func(input),
			([res, rest]) => Right([f(res), rest]),
			Left));

	return func;
};

const parse = (toks: Token[]) => {
	const satisfy = (pred: (t: Token) => boolean) =>
		parser((input) =>
			input.length
			? caseof(input, ([head, ...rest]) =>
				pred(head)
				? { right: [head, rest] }
				: { left: [`unexpected ${tokenToString(head)}`] })
			: { left: [`unexpected end-of-input`] }
		);
	
	const isToken = (t: Token) => satisfy(t2 => tokenEqual(t, t2));
	const name = satisfy(t2 => t2.type === 'name').to(t => t.value!);

	const forall = parser(
		isToken({ type: 'forall' })
		.seq(name).to(tail).to(head)
		.seq(isToken({ type: 'dot' })).to(head)
	);

	return forall(toks);
};

const language = (source: string): Either<string, string> | null => {
	source = source.trim();
	if (source.length === 0) return null;

	const toks = tokenize(source);
	if ('left' in toks) return toks;

	const ast = parse(toks.right);
	if ('left' in ast) return Left(ast.left[0]);

	return { right: ast.right[0] };
	// return { left: `toks: ${toks.right.map(tokenToString).join(', ')}` };
};

document.body.append(E("main", el => {
	const inputState = new BasicState(`forall a b.\(f:(a->a)->a->b).\(x:a).f id{a} x`);
	const resultState = new BasicState<Either<string, string> | null>(null);
	effectNow(inputState, (value) => resultState.update(language(value)));
	el.append(
		E("div", el => {
			el.classList.add("input-wrap");
			el.append(E("input", el => {
				el.placeholder = "Code";
				el.classList.add("code");
				effectNow(inputState, setter(el, 'value'));
				el.oninput = () => inputState.update(el.value);
			}));
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
	);
}));
