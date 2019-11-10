"use strict";
/// <reference path="node_modules/rxjs/index.d.ts"/>
Object.defineProperty(exports, "__esModule", { value: true });
const index_1 = require("./node_modules/rxjs/index");
const index_2 = require("./node_modules/rxjs/operators/index");
// const { range } = rxjs;
// const {map,filter} = 'rxjs/operators';
// const { range } = Obs;
// const { map, filter } = rxjs.operators;
index_1.range(1, 200).pipe(index_2.filter(x => x % 2 === 1), index_2.map(x => x + x)).subscribe(x => console.log(x));
window.onload = () => {
    var btn = document.createElement("BUTTON"); // Create a <button> element
    btn.innerHTML = "CLICK ME"; // Insert text
    document.body.appendChild(btn);
};
