let Core := import "musi:core";
let Int := Core.Int;
let String := Core.String;
let Unit := Core.Unit;

export opaque let Runtime := effect {
  let envGet (name : String) : String;
  let envHas (name : String) : Int;
  let envSet (name : String, value : String) : Int;
  let envRemove (name : String) : Int;

  let processArgCount () : Int;
  let processArgAt (index : Int) : String;
  let processCwd () : String;
  let processRun (command : String) : Int;
  let processExit (code : Int) : Unit;

  let timeNowUnixMs () : Int;
  let timeMonotonicMs () : Int;
  let timeSleepMs (ms : Int) : Unit;
  let randomInt () : Int;
  let randomIntInRange (lowerBound : Int, upperBound : Int) : Int;
  let randomBool () : Int;
  let randomFloat01 () : Float;

  let logInfo (message : String) : Unit;
  let logWrite (level : Int, message : String) : Unit;
  let ioPrint (text : String) : Unit;
  let ioPrintLine (text : String) : Unit;
  let ioPrintError (text : String) : Unit;
  let ioPrintErrorLine (text : String) : Unit;
  let ioReadLine () : String;

  let fsReadText (path : String) : String;
  let fsWriteText (path : String, text : String) : Unit;
  let fsExists (path : String) : Int;
  let fsAppendText (path : String, text : String) : Int;
  let fsRemove (path : String) : Int;
  let fsCreateDirAll (path : String) : Int;

  let textLength (value : String) : Int;
  let textTrim (value : String) : String;
  let textToLowerCase (value : String) : String;
  let textToUpperCase (value : String) : String;
  let textContains (value : String, needle : String) : Int;
  let textStartsWith (value : String, prefix : String) : Int;
  let textEndsWith (value : String, suffix : String) : Int;

  let pathJoin (left : String, right : String) : String;
  let pathNormalize (value : String) : String;
  let pathDirname (value : String) : String;
  let pathBasename (value : String) : String;
  let pathExtname (value : String) : String;
  let pathIsAbsolute (value : String) : Int;

  let jsonIsValid (source : String) : Int;
  let jsonNormalize (source : String) : String;

  let hexEncode (source : String) : String;
  let hexDecode (source : String) : String;
  let hexIsValid (source : String) : Int;

  let base64Encode (source : String) : String;
  let base64Decode (source : String) : String;
  let base64IsValid (source : String) : Int;

  let utf8Encode (source : String) : String;
  let utf8Decode (source : String) : String;
  let utf8IsValid (source : String) : Int;
};

export let envGet (name : String) : String := perform Runtime.envGet(name);
export let envHas (name : String) : Int := perform Runtime.envHas(name);
export let envSet (name : String, value : String) : Int := perform Runtime.envSet(name, value);
export let envRemove (name : String) : Int := perform Runtime.envRemove(name);

export let processArgCount () : Int := perform Runtime.processArgCount();
export let processArgAt (index : Int) : String := perform Runtime.processArgAt(index);
export let processCwd () : String := perform Runtime.processCwd();
export let processRun (command : String) : Int := perform Runtime.processRun(command);
export let processExit (code : Int) : Unit := perform Runtime.processExit(code);

export let timeNowUnixMs () : Int := perform Runtime.timeNowUnixMs();
export let timeMonotonicMs () : Int := perform Runtime.timeMonotonicMs();
export let timeSleepMs (ms : Int) : Unit := perform Runtime.timeSleepMs(ms);
export let randomInt () : Int := perform Runtime.randomInt();
export let randomIntInRange (lowerBound : Int, upperBound : Int) : Int :=
  perform Runtime.randomIntInRange(lowerBound, upperBound);
export let randomBool () : Int := perform Runtime.randomBool();
export let randomFloat01 () : Float := perform Runtime.randomFloat01();

export let logInfo (message : String) : Unit := perform Runtime.logInfo(message);
export let logWrite (level : Int, message : String) : Unit := perform Runtime.logWrite(level, message);
export let ioPrint (text : String) : Unit := perform Runtime.ioPrint(text);
export let ioPrintLine (text : String) : Unit := perform Runtime.ioPrintLine(text);
export let ioPrintError (text : String) : Unit := perform Runtime.ioPrintError(text);
export let ioPrintErrorLine (text : String) : Unit := perform Runtime.ioPrintErrorLine(text);
export let ioReadLine () : String := perform Runtime.ioReadLine();

export let fsReadText (path : String) : String := perform Runtime.fsReadText(path);
export let fsWriteText (path : String, text : String) : Unit :=
  perform Runtime.fsWriteText(path, text);
export let fsExists (path : String) : Int := perform Runtime.fsExists(path);
export let fsAppendText (path : String, text : String) : Int :=
  perform Runtime.fsAppendText(path, text);
export let fsRemove (path : String) : Int := perform Runtime.fsRemove(path);
export let fsCreateDirAll (path : String) : Int := perform Runtime.fsCreateDirAll(path);

export let textLength (value : String) : Int := perform Runtime.textLength(value);
export let textTrim (value : String) : String := perform Runtime.textTrim(value);
export let textToLowerCase (value : String) : String := perform Runtime.textToLowerCase(value);
export let textToUpperCase (value : String) : String := perform Runtime.textToUpperCase(value);
export let textContains (value : String, needle : String) : Int :=
  perform Runtime.textContains(value, needle);
export let textStartsWith (value : String, prefix : String) : Int :=
  perform Runtime.textStartsWith(value, prefix);
export let textEndsWith (value : String, suffix : String) : Int :=
  perform Runtime.textEndsWith(value, suffix);

export let pathJoin (left : String, right : String) : String := perform Runtime.pathJoin(left, right);
export let pathNormalize (value : String) : String := perform Runtime.pathNormalize(value);
export let pathDirname (value : String) : String := perform Runtime.pathDirname(value);
export let pathBasename (value : String) : String := perform Runtime.pathBasename(value);
export let pathExtname (value : String) : String := perform Runtime.pathExtname(value);
export let pathIsAbsolute (value : String) : Int := perform Runtime.pathIsAbsolute(value);

export let jsonIsValid (source : String) : Int := perform Runtime.jsonIsValid(source);
export let jsonNormalize (source : String) : String := perform Runtime.jsonNormalize(source);

export let hexEncode (source : String) : String := perform Runtime.hexEncode(source);
export let hexDecode (source : String) : String := perform Runtime.hexDecode(source);
export let hexIsValid (source : String) : Int := perform Runtime.hexIsValid(source);

export let base64Encode (source : String) : String := perform Runtime.base64Encode(source);
export let base64Decode (source : String) : String := perform Runtime.base64Decode(source);
export let base64IsValid (source : String) : Int := perform Runtime.base64IsValid(source);

export let utf8Encode (source : String) : String := perform Runtime.utf8Encode(source);
export let utf8Decode (source : String) : String := perform Runtime.utf8Decode(source);
export let utf8IsValid (source : String) : Int := perform Runtime.utf8IsValid(source);
