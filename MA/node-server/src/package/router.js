import Router from 'koa-router';
import packageStore from './store';
import { broadcast } from "../utils";

export const router = new Router();

const pageSize = 10;

router.get('/:page', async (ctx) => {
  const response = ctx.response;
  const userId = ctx.state.user._id;
  const page = parseInt(ctx.params.page) || 0;

  const packages = await packageStore.find({ userId });
  packages.sort((a, b) => a._id - b._id);

  response.body = packages.slice(page * pageSize, pageSize);
  response.status = 200; // ok
});

// router.get('/:id', async (ctx) => {
//   const userId = ctx.state.user._id;
//   const note = await packageStore.findOne({ _id: ctx.params.id });
//   const response = ctx.response;
//   if (note) {
//     if (note.userId === userId) {
//       response.body = note;
//       response.status = 200; // ok
//     } else {
//       response.status = 403; // forbidden
//     }
//   } else {
//     response.status = 404; // not found
//   }
// });

const createPackage = async (ctx, pkg, response) => {
  try {
    const userId = ctx.state.user._id;
    console.log("userId", userId);
    pkg.userId = userId;
    response.body = await packageStore.insert(pkg);
    response.status = 201; // created
    broadcast(userId, { type: 'created', payload: pkg });
  } catch (err) {
    response.body = { message: err.message };
    response.status = 400; // bad request
  }
};

router.post('/', async ctx => await createPackage(ctx, ctx.request.body, ctx.response));

router.put('/:id', async (ctx) => {
  const pkg = ctx.request.body;
  const id = ctx.params.id;
  const pkgId = pkg._id;
  const response = ctx.response;
  if (pkgId && pkgId !== id) {
    response.body = { message: 'Param id and body _id should be the same' };
    response.status = 400; // bad request
    return;
  }
  if (!pkgId) {
    await createNote(ctx, pkg, response);
  } else {
    const userId = ctx.state.user._id;
    pkg.userId = userId;
    const updatedCount = await packageStore.update({ _id: id }, pkg);
    if (updatedCount === 1) {
      response.body = pkg;
      response.status = 200; // ok
      broadcast(userId, { type: 'updated', payload: pkg });
    } else {
      response.body = { message: 'Resource no longer exists' };
      response.status = 405; // method not allowed
    }
  }
});

router.del('/:id', async (ctx) => {
  const userId = ctx.state.user._id;
  const pkg = await packageStore.findOne({ _id: ctx.params.id });
  if (pkg && userId !== pkg.userId) {
    ctx.response.status = 403; // forbidden
  } else {
    await packageStore.remove({ _id: ctx.params.id });
    ctx.response.status = 204; // no content
  }
});
