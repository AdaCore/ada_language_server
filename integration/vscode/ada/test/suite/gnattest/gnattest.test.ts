import { suite } from 'mocha';
import { activate } from '../utils';

suite('GNATtest Integration Tests', function () {
    this.beforeAll(async () => {
        await activate();
    });
});
