// const Koa = require('koa');
// const app = new Koa();
// const server = require('http').createServer(app.callback());
// const WebSocket = require('ws');
// const wss = new WebSocket.Server({ server });
// const Router = require('koa-router');
// const cors = require('koa-cors');
// const bodyparser = require('koa-bodyparser');

// app.use(bodyparser());
// app.use(cors());
// app.use(async (ctx, next) => {
//   const start = new Date();
//   await next();
//   const ms = new Date() - start;
//   console.log(`${ctx.method} ${ctx.url} ${ctx.response.status} - ${ms}ms`);
// });

// app.use(async (ctx, next) => {
//   await new Promise(resolve => setTimeout(resolve, 2000));
//   await next();
// });

// app.use(async (ctx, next) => {
//   try {
//     await next();
//   } catch (err) {
//     ctx.response.body = { issue: [{ error: err.message || 'Unexpected error' }] };
//     ctx.response.status = 500;
//   }
// });

// class Item {
//   constructor({ id, title, date, numberOfPeople, isApproved }) {
//     this.id = id;
//     this.title = title;
//     this.date = date;
//     this.numberOfPeople = numberOfPeople;
//     this.isApproved = isApproved;
//   }
// }

// const items = [];
// for (let i = 0; i < 3; i++) {
//   items.push(new Item({ id: `${i}`, title: `Event ${i}`, date: new Date(Date.now()),numberOfPeople: 50, isApproved: false }));
// }
// let lastUpdated = items[items.length - 1].date;
// let lastId = items[items.length - 1].id;
// const pageSize = 10;

// const broadcast = data =>
//   wss.clients.forEach(client => {
//     if (client.readyState === WebSocket.OPEN) {
//       client.send(JSON.stringify(data));
//     }
//   });

// const router = new Router();

// router.get('/specialEvent', ctx => {
//   ctx.response.body = items;
//   ctx.response.status = 200;
// });

// router.get('/specialEvent/:id', async (ctx) => {
//   const itemId = ctx.request.params.id;
//   const item = items.find(item => itemId === item.id);
//   if (item) {
//     ctx.response.body = item;
//     ctx.response.status = 200; // ok
//   } else {
//     ctx.response.body = { issue: [{ warning: `item with id ${itemId} not found` }] };
//     ctx.response.status = 404; // NOT FOUND (if you know the resource was deleted, then return 410 GONE)
//   }
// });

// const createItem = async (ctx) => {
//   const item = ctx.request.body;
//   if (!item.title || item.title=="") { // validation
//     ctx.response.body = { issue: [{ error: 'Title is incorrect' }] };
//     ctx.response.status = 400; //  BAD REQUEST
//     return;
//   }
//   item.id = `${parseInt(lastId) + 1}`;
//   lastId = item.id;
//   items.push(item);
//   ctx.response.body = item;
//   ctx.response.status = 201; // CREATED
//   broadcast({ event: 'created', payload: { item } });
// };

// router.post('/specialEvent', async (ctx) => {
//   await createItem(ctx);
// });

// router.put('/specialEvent/:id', async (ctx) => {
//   const id = ctx.params.id;
//   const item = ctx.request.body;
//   const itemId = item.id;
//   if (itemId && id !== item.id) {
//     ctx.response.body = { issue: [{ error: `Param id and body id should be the same` }] };
//     ctx.response.status = 400; // BAD REQUEST
//     return;
//   }
//   if (!itemId) {
//     await createItem(ctx);
//     return;
//   }
//   const index = items.findIndex(item => item.id === id);
//   if (index === -1) {
//     ctx.response.body = { issue: [{ error: `item with id ${id} not found` }] };
//     ctx.response.status = 400; // BAD REQUEST
//     return;
//   }
//   items[index] = item;
//   lastUpdated = new Date();
//   ctx.response.body = item;
//   ctx.response.status = 200; // OK
//   broadcast({ event: 'updated', payload: { item } });
// });

// router.del('/specialEvent/:id', ctx => {
//   const id = ctx.params.id;
//   const index = items.findIndex(item => id === item.id);
//   if (index !== -1) {
//     const item = items[index];
//     items.splice(index, 1);
//     lastUpdated = new Date();
//     broadcast({ event: 'deleted', payload: { item } });
//   }
//   ctx.response.status = 204; // no content
// });

// // setInterval(() => {
// //   lastUpdated = new Date();
// //   lastId = `${parseInt(lastId) + 1}`;
// //   const item = new Item({ id: lastId, text: `event ${lastId}`, date: new Date(), isApproved: false, numberOfPeople: 50 });
// //   items.push(item);
// //   console.log(`
// //    ${item.text}`);
// //   broadcast({ event: 'created', payload: { item } });
// // }, 1000);

// app.use(router.routes());
// app.use(router.allowedMethods());

// server.listen(3000);


import Koa from 'koa';
import WebSocket from 'ws';
import http from 'http';
import Router from 'koa-router';
import bodyParser from "koa-bodyparser";
import { timingLogger, exceptionHandler, jwtConfig, initWss, verifyClient } from './utils';
import { router as specialEventRouter } from './specialEvent';
import { router as authorizationRouter } from './authorization';
import jwt from 'koa-jwt';
import cors from '@koa/cors';

const app = new Koa();
const server = http.createServer(app.callback());
const wss = new WebSocket.Server({ server });
initWss(wss);

app.use(cors());
app.use(timingLogger);
app.use(exceptionHandler);
app.use(bodyParser());

const prefix = '/api';

// public
const publicApiRouter = new Router({ prefix });
publicApiRouter
  .use('/auth', authorizationRouter.routes());
app
  .use(publicApiRouter.routes())
  .use(publicApiRouter.allowedMethods());

app.use(jwt(jwtConfig));

// protected
const protectedApiRouter = new Router({ prefix });
protectedApiRouter
  .use('/specialEvent', specialEventRouter.routes());
app
  .use(protectedApiRouter.routes())
  .use(protectedApiRouter.allowedMethods());

server.listen(3000);
console.log('started on port 3000');
