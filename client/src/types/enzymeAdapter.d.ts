declare module 'enzyme-adapter-react-16';

import * as enzyme from 'enzyme';
import * as Adapter from 'enzyme-adapter-react-16';

// tslint:disable-next-line:no-any
(enzyme as any).configure({ adapter: new Adapter() });
