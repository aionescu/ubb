export const exceptionHandler = async (ctx, next) => {
  try {
    return await next();
  } catch (err) {
    ctx.body = { message: err.message || 'Unexpected error.' };
    ctx.status = err.status || 500;
  }
};

export const timingLogger = async (ctx, next) => {
  const start = Date.now();
  await next();
  const end = Date.now();

  const d = new Date(start);
  const hour = `${d.getHours()}:${d.getMinutes()}:${d.getSeconds()}.${d.getMilliseconds()}`;

  console.log(`${ctx.method} ${ctx.url} => ${ctx.response.status}, ${end - start}ms @ ${hour}`);
};
