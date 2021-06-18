function port ( app ) {


  app.ports.sendRequestPointerLock.subscribe ( () => {
    const canvas = document.querySelector( "canvas" );
    function onpointerlockchange () {
      const locked = canvas === document.pointerLockElement;
      app.ports.resvRequestPointerLock.send ( locked );
      if ( !locked ) document.removeEventListener ( "pointerlockchange", onpointerlockchange );
    }
    document.addEventListener ( "pointerlockchange", onpointerlockchange );
    canvas.requestPointerLock ();
  });


  app.ports.sendLocalStorage.subscribe ( string =>
    localStorage.setItem ( "three.storage", string ) );


  return app;
}
