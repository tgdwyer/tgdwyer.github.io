/// <reference path="node_modules/rxjs/index.d.ts"/>

import { range } from "./node_modules/rxjs/index";
import {filter,map} from "./node_modules/rxjs/operators/index"
// const { range } = rxjs;
// const {map,filter} = 'rxjs/operators';
// const { range } = Obs;
// const { map, filter } = rxjs.operators;

range(1, 200).pipe(
  filter(x => x % 2 === 1),
  map(x => x + x)
).subscribe(x => console.log(x));

window.onload = ()=>{
    var btn = document.createElement("BUTTON");   // Create a <button> element
    btn.innerHTML = "CLICK ME";                   // Insert text
    document.body.appendChild(btn);
}