import 'normalize.css';
import 'bulma/css/bulma.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import './main.css';

Elm.Main.init({
  node: document.getElementById('root'),
});

registerServiceWorker();
