import { BasicState, effectNow, joinedState } from "statec";
import { E } from "./html";

document.head.appendChild(E("style", el => {
	el.innerHTML = `
body { padding: 1rem; }
button { margin: 5px 0; margin-left: 5px; }
input ~ input:not([type]), input ~ input[type="text"] { margin-left: 5px; }
label { margin-left: 5px; }
`;
}));

import './test1.ts';
