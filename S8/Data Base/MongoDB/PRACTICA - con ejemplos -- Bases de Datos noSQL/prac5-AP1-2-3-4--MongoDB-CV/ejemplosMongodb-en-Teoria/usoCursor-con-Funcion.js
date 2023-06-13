var cursor = db.ultimasAventuras.find({ },{})

cursor.forEach( 
  function(item) 
      { print(tojson(item))  }
);

//------------ otro uso:
 cursor.hasNext() // "false"  si ya no hay más elementos
cursor.hasNext() // "true" s todavía quedan elementos por recorrer

