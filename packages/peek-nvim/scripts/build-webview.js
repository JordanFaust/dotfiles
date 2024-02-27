import { emit } from './build.out.js';

const result = Promise.all([
  emit('client/src/script.ts', 'public/script.bundle.js'),
]);

result.catch(console.error);
