import 'normalize.css';
import {Elm} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
//import './main.css';
//import './animations.css';
import './styles.css';

Elm.Main.init({
  node: document.getElementById('root'),
});

registerServiceWorker();
