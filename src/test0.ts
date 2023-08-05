import { BasicState, effectNow, joinedState } from "statec";
import { E, EventState, icons, setter } from "./html";

const greetingsState = new BasicState([
	new BasicState('Greetings'),
	new BasicState('Hi'),
	new BasicState('Hello'),
]);

const nameState = new BasicState('');
const greetingIndexState = new BasicState(0);
greetingsState.effect(greetings => {
	if (greetingIndexState.get() >= greetings.length) {
		greetingIndexState.update(greetings.length - 1);
	}
});

const main = document.body.appendChild(E('main'));
main.append(
	E('input', el => {
		el.placeholder = "a name";
		effectNow(nameState, setter(el, 'value'));
		el.oninput = () => nameState.update(el.value);
	}),
	E('p', el => {
		effectNow(
			joinedState(nameState, greetingIndexState, greetingsState),
			([name, greetingIndex, greetings]) => {
				effectNow(greetings[greetingIndex], (greeting) => {
					el.textContent = `${greeting}, ${name}`;
				});
			}
		);
	}),
	E('ul', el => {
		effectNow(greetingsState, (greetings) => {
			el.replaceChildren(
				...greetings.flatMap((greeting, index) => E('li', el => {
					const isEditingState = new BasicState<boolean>(false);
					el.append(
						E('input', el => {
							el.type = 'radio';
							el.name = `greetingChoice${index}`;
			
							effectNow(greetingIndexState, (greetingIndex) => {
								el.checked = (greetingIndex === index);
							});
			
							el.oninput = () => greetingIndexState.update(index);
						}),
						E('label', el => {
							el.htmlFor = `greetingChoice${index}`;
							effectNow(greeting, setter(el, 'textContent'));
							effectNow(isEditingState, isEditing => {
								el.style.display = isEditing ? 'none' : 'inline';
							});
						}),
						E('input', el => {
							el.type = 'text';
							el.placeholder = "greeting";
							effectNow(greeting, value => {
								el.value = value;
							});
							effectNow(isEditingState, isEditing => {
								el.style.display = isEditing ? 'inline' : 'none';
							});
							el.oninput = () => greeting.update(el.value);
						}),
						E('button', el => {
							el.type = 'button';
							el.innerHTML = icons.close({ width: "10px", height: "10px" });
							el.disabled = (greetings.length === 1);
							el.onclick = () => greetingsState.update(
								greetings.filter((_, i) => i != index)
							);
						}),
						E('button', el => {
							el.type = 'button';
							el.innerHTML = icons.edit({ width: "10px", height: "10px" });
							el.onclick = () => {
								isEditingState.update(!isEditingState.get());
							}
						}),
					);
				}
			)));
		});
	}),
	E('div', el => {
		const valueState = new BasicState('');
		const submit = new EventState();

		submit.listen(() => {
			greetingsState.update([...greetingsState.get(), new BasicState(valueState.get())]);
			valueState.update('');
		});

		el.append(
			E('input', el => {
				el.placeholder = "greeting";
				el.oninput = () => valueState.update(el.value);
				effectNow(valueState, value => { el.value = value.trim(); });
				el.onkeydown = (ev) => {
					if (ev.key === 'Enter') {
						submit.fire();
					}
				};
			}),
			E('button', el => {
				el.type = 'button';
				el.innerHTML = icons.plus({ width: "10px", height: "10px" });
				effectNow(valueState, value => {
					el.disabled = (value.trim() === '');
				});
				el.onclick = () => submit.fire();
			}),
		);
	}),
);
