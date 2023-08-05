import { BasicState } from "statec";

export class EventState extends BasicState<undefined> {
	constructor() { super(undefined); }
	listen(func: () => void) { this.effect(func); }
	fire() { this.update(undefined); }
}

export function E<K extends keyof HTMLElementTagNameMap>(
	tagName: K,
	handler?: (el: HTMLElementTagNameMap[K]) => void
): HTMLElementTagNameMap[K] {
	const el = document.createElement(tagName);
	handler?.(el);
	return el;
}

export const setter = <T, K extends keyof T>(ref: T, k: K) =>
	(v: T[K]) => { ref[k] = v; }

export type Dimension = `${number}${'%' | 'px' | 'rem'}` | number;
export type Dimensions = { width: Dimension, height: Dimension };
export const wrapIconSVG = (src: string, { width, height }: Dimensions) =>
	`<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32" ` +
		`width="${width}" height="${height}" fill="none" stroke="currentcolor" ` +
		`stroke-linecap="round" stroke-linejoin="round" stroke-width="4">${src}</svg>`;

		export const iconFunc = (src: string) => ({ width = "10px", height = "10px" }: Partial<Dimensions> = {}) =>
	wrapIconSVG(src, { width, height });

export const icons = {
	close: iconFunc(`<path d="M2 30 L30 2 M30 30 L2 2" />`),
	plus: iconFunc(`<path d="M16 2 L16 30 M2 16 L30 16" />`),
	edit: iconFunc(`<path d="M30 7 L25 2 5 22 3 29 10 27 Z M21 6 L26 11 Z M5 22 L10 27 Z" />`),
};
