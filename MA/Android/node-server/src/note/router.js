import Router from 'koa-router';
import noteStore from './store';
import { broadcast } from "../utils";

export const router = new Router();

router.get('/', async (ctx) => {
  const response = ctx.response;
  const userId = ctx.state.user._id;
  response.body = await noteStore.find({ userId });
  response.status = 200; // ok
});

router.get('/:id', async (ctx) => {
  const userId = ctx.state.user._id;
  const note = await noteStore.findOne({ _id: ctx.params.id });
  const response = ctx.response;
  if (note) {
    if (note.userId === userId) {
      response.body = note;
      response.status = 200; // ok
    } else {
      response.status = 403; // forbidden
    }
  } else {
    response.status = 404; // not found
  }
});

const createNote = async (ctx, note, response) => {
  try {
    const userId = ctx.state.user._id;
    note.userId = userId;
    response.body = await noteStore.insert(note);
    response.status = 201; // created
    broadcast(userId, { type: 'created', payload: note });
  } catch (err) {
    response.body = { message: err.message };
    response.status = 400; // bad request
  }
};

router.post('/', async ctx => await createNote(ctx, ctx.request.body, ctx.response));

router.put('/:id', async (ctx) => {
  const note = ctx.request.body;
  const id = ctx.params.id;
  const noteId = note._id;
  const response = ctx.response;
  if (noteId && noteId !== id) {
    response.body = { message: 'Param id and body _id should be the same' };
    response.status = 400; // bad request
    return;
  }
  if (!noteId) {
    await createNote(ctx, note, response);
  } else {
    const userId = ctx.state.user._id;
    note.userId = userId;
    const updatedCount = await noteStore.update({ _id: id }, note);
    if (updatedCount === 1) {
      response.body = note;
      response.status = 200; // ok
      broadcast(userId, { type: 'updated', payload: note });
    } else {
      response.body = { message: 'Resource no longer exists' };
      response.status = 405; // method not allowed
    }
  }
});

router.del('/:id', async (ctx) => {
  const userId = ctx.state.user._id;
  const note = await noteStore.findOne({ _id: ctx.params.id });
  if (note && userId !== note.userId) {
    ctx.response.status = 403; // forbidden
  } else {
    await noteStore.remove({ _id: ctx.params.id });
    ctx.response.status = 204; // no content
  }
});
