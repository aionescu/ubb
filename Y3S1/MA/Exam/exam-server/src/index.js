const Koa = require('koa');
const app = new Koa();
const server = require('http').createServer(app.callback());
const Router = require('koa-router');
const cors = require('koa-cors');
const bodyparser = require('koa-bodyparser');

app.use(bodyparser());
app.use(cors());

app.use(async function (ctx, next) {
  const start = new Date();
  await next();
  const ms = new Date() - start;
  console.log(`${ctx.method} ${ctx.url} - ${ms}ms`);
});

app.use(async (ctx, next) => {
  await new Promise(resolve => setTimeout(resolve, 1000));
  await next();
});

const tasks = Array.from(Array(30).keys())
  .map(id => ({ id, text: `t${id}`, status: 'active', version: 1 }));

const router = new Router();
router.get('/task', ctx => {
  const q = ctx.request.query.q;
  ctx.response.body = tasks.filter(task => q ? task.text.indexOf(q) !== -1 : true);
  ctx.response.status = 200;
});

router.put('/task/:id', ctx => {
  const task = ctx.request.body;
  const id = parseInt(ctx.params.id);
  const index = tasks.findIndex(task => task.id === id);
  if (id !== task.id || index === -1) {
    ctx.response.body = { text: 'Task not found' };
    ctx.response.status = 400;
  } else if (task.version < tasks[index].version) {
    ctx.response.body = { text: 'Version conflict' };
    ctx.response.status = 409;
  } else {
    task.version++;
    tasks[index] = task;
    ctx.response.body = task;
    ctx.response.status = 200;
  }
});

app.use(router.routes());
app.use(router.allowedMethods());

server.listen(3000);
