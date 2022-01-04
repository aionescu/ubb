export const exceptionHandler = async (ctx, next) => {
  try {
    return await next();
  } catch (err) {
    ctx.body = { message: err.message || 'Unexpected error.' };
    ctx.status = err.status || 500;
  }
};

function pad(n) {
  return n < 10 ? `0${n}` : n;
}

function fmtDate(date) {
  const d = new Date(date);

  const year = d.getFullYear();
  const month = pad(d.getMonth() + 1);
  const day = pad(d.getDate());
  const hour = pad(d.getHours());
  const minute = pad(d.getMinutes());
  const second = pad(d.getSeconds());

  return `${year}-${month}-${day} ${hour}:${minute}:${second}`;
}

export const timingLogger = async (ctx, next) => {
  const start = Date.now();
  await next();
  console.log(`${ctx.method} ${ctx.url} => ${ctx.response.status}, ${Date.now() - start}ms @ ${fmtDate(start)}`);
};
