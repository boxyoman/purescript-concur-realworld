var Main = require("../output/Main/index.js");

function main () {
  Main.main();
}

if (module.hot) {
  module.hot.accept( function () {
    console.log("reloaded, running main again");
    main();
  });
}

main();
