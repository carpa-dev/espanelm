import 'normalize.css';
import {Elm} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import '../styles/styles.css';

Elm.Main.init({
  node: document.getElementById('root'),
});

registerServiceWorker();
