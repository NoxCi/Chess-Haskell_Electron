const electron = require('electron');
const {ipcRenderer} = electron;
const ul = document.querySelector('ul');
const form = document.querySelector('form');

var fs = require('fs');
var http = require('http');

fileP_I = 'shared_files/Ohs_Ijs.txt'
fileP_O = 'shared_files/Ihs_Ojs.txt'
tablero = ''
flag = false
r = 0

//setInterval(output, 1000, fileP,'j ........');
setInterval(input, 100, fileP_I);

/******IO code*****/
function input(filePath){
  fs.readFile(filePath,'utf8',(err, data) => {
    if(err) throw err;
    tablero = data
  });
}

function output(filePath, text) {
    fs.writeFile(filePath, text, function (err) {
      if (err) throw err;
    });
}
/******IO code*****/

/******Electron code*****/
function mkUl(data) {
  ul.innerHTML = '';
  ul.className = 'collection';
  const li = document.createElement('li');
  li.className = 'collection-item';
  const itemText = document.createTextNode(data);
  li.appendChild(itemText);
  ul.appendChild(li);
}
/******Electron code*****/

/******p5 code******/
function setup() {
  output(fileP_O,'.')
  w = windowWidth - 25
  createCanvas(w, w);
  background(255);
  textAlign(CENTER);
  textSize(24);

  b = 255;
  n = 0;
  r = Math.floor(w/10);

  for (var i = 0; i < 10; i++) {
    for (var j = 0; j < 10; j++) {
      if (i == 0 || i == 9|| j == 0 ||j == 9) continue;
      if((j % 2 == 0 && i % 2 == 0) || (j % 2 != 0 && i % 2 != 0)){
        noStroke();
        fill(150);
        rect(j*r, i*r, r, r);
      } else {
        noStroke();
        fill(75);
        rect(j*r, i*r, r, r);
      }
    }
  }
}

function draw() {
  rH = Math.floor(r/2);
  if (tablero != "") {
    lines = tablero.split('\n');
    turno = lines[0].slice(0,1);
    msg = lines[1];
    mkUl(msg);
    for (var i = 0; i < 10; i++) {
      contenidoTab = lines[i+2].split(' ')
      for (var j = 0; j < 10; j++) {

        //aqui dibujamos las letras
        if (i == 0 || i == 9) {
          strokeWeight(2);
          stroke(0);
          fill(0);
          text(contenidoTab[j].slice(2,3),j*r + rH,i*r + rH+10);
          continue;
        }

        //dibujamos los numeros
        if (j == 0 || j == 9) {
          strokeWeight(2);
          stroke(0);
          fill(0);
          text(contenidoTab[0].slice(2,3),j*r + rH,i*r + rH+10);
          continue;
        }

        //dibujamos las casillas
        p = contenidoTab[j]
        if((j % 2 == 0 && i % 2 == 0) || (j % 2 != 0 && i % 2 != 0)){
          noStroke();
          fill(150);
          rect(j*r, i*r, r, r);
        } else {
          noStroke();
          fill(75);
          rect(j*r, i*r, r, r);
        }

        //dibujamos las piezas
        if (p.slice(1,2) != '_') {
          color = 0;
          if (p.slice(1,2) == 'B') color = 255;
          strokeWeight(2);
          stroke(color);
          fill(color);
          text(p.slice(3,4),j*r + rH,i*r + rH+10);
        }

        //dibujamos los posibles movimientos
        if (p.slice(0,1) == ':') {
          color = 0;
          if (turno == '1') color = 255;
          strokeWeight(4);
          stroke(color);
          noFill();
          rect(j*r, i*r, r, r);
        }
      }
    }
  }
}

function mousePressed() {
  var x = Math.floor(mouseX/r) + 1
  var y = Math.floor(mouseY/r) + 1
  if (x != 1 && x != 10 && y != 1 && y != 10) {
    if (flag) output(fileP_O,'.' + (x-1) + '.' + (y-1));
    else output(fileP_O,':' + (x-1) + '.' + (y-1));
    flag = !flag;
  }

}
/******p5 code******/
