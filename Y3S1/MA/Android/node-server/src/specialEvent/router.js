import Router from 'koa-router';
import SpecialEventStore from './store';
import { broadcast } from "../utils";

export const router = new Router();

router.get('/', async (ctx) => {
  const response = ctx.response;
  const userId = ctx.state.user._id;
  response.body = await SpecialEventStore.find({ userId });
  response.status = 200; // ok
});

router.get('/:id', async (ctx) => {
  const userId = ctx.state.user._id;
  const specialEvent = await SpecialEventStore.findOne({ _id: ctx.params.id });
  const response = ctx.response;
  if (specialEvent) {
    if (specialEvent.userId === userId) {
      response.body = specialEvent;
      response.status = 200; // ok
    } else {
      response.status = 403; // forbidden
    }
  } else {
    response.status = 404; // not found
  }
});

const createSpecialEvent = async (ctx, specialEvent, response) => {
  try {
    const userId = ctx.state.user._id;
    specialEvent.userId = userId;
    response.body = await SpecialEventStore.insert(specialEvent);
    response.status = 201; // created
    broadcast(userId, { type: 'created', payload: specialEvent });
  } catch (err) {
    response.body = { message: err.message };
    response.status = 400; // bad request
  }
};

router.post('/', async ctx => await createSpecialEvent(ctx, ctx.request.body, ctx.response));

router.put('/:id', async (ctx) => {
  const specialEvent = ctx.request.body;
  const id = ctx.params.id;
  const specialEventId = specialEvent._id;
  const response = ctx.response;
  if (specialEventId && specialEventId !== id) {
    response.body = { message: 'Param id and body _id should be the same' };
    response.status = 400; // bad request
    return;
  }
  if (!specialEventId) {
    await createSpecialEvent(ctx, specialEvent, response);
  } else {
    const userId = ctx.state.user._id;
    specialEvent.userId = userId;
    const updatedCount = await SpecialEventStore.update({ _id: id }, specialEvent);
    if (updatedCount === 1) {
      response.body = specialEvent;
      response.status = 200; // ok
      broadcast(userId, { type: 'updated', payload: specialEvent });
    } else {
      response.body = { message: 'Resource no longer exists' };
      response.status = 405; // method not allowed
    }
  }
});

router.del('/:id', async (ctx) => {
  const userId = ctx.state.user._id;
  const note = await SpecialEventStore.findOne({ _id: ctx.params.id });
  if (note && userId !== note.userId) {
    ctx.response.status = 403; // forbidden
  } else {
    await SpecialEventStore.remove({ _id: ctx.params.id });
    ctx.response.status = 204; // no content
  }
});
