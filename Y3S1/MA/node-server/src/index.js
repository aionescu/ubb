import Koa from 'koa';
import WebSocket from 'ws';
import http from 'http';
import Router from 'koa-router';
import bodyParser from "koa-bodyparser";
import { timingLogger, exceptionHandler, jwtConfig, initWss, verifyClient } from './utils';
import { router as packageRouter } from './package';
import { router as authRouter } from './auth';
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
  .use('/auth', authRouter.routes());
app
  .use(publicApiRouter.routes())
  .use(publicApiRouter.allowedMethods());

app.use(jwt(jwtConfig));

// protected
const protectedApiRouter = new Router({ prefix });
protectedApiRouter
  .use('/item', packageRouter.routes());
app
  .use(protectedApiRouter.routes())
  .use(protectedApiRouter.allowedMethods());

server.listen(3000);
console.log('started on port 3000');
