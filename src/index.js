import 'normalize.css';
import './main.css';
import {Elm} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import 'bulma/css/bulma.css';

import 'tailwindcss/dist/tailwind.min.css';

Elm.Main.init({
  node: document.getElementById('root'),
});

registerServiceWorker();
