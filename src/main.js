import { Elm } from './Main.elm'

let flags
try {
  flags = JSON.parse(window.localStorage.getItem("model"))
}
catch ( error ) {
  console.log(error)
}
console.log(flags)
flags = flags ? flags : ""

let app = Elm.Main.init({
  node: document.getElementById('elm'),
  flags: flags
})

app.ports.store.subscribe(data => {
  console.log(data)
  window.localStorage.setItem("model", JSON.stringify(data))
})
