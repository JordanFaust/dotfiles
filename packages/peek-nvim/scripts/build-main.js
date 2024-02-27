import { emit } from './build.out.js';

const result = Promise.all([
  emit('app/src/main.ts', 'public/main.bundle.js')
]);

result.catch(console.error);
