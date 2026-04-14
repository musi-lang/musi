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

export let envGet (name : String) : String := request Runtime.envGet(name);
export let envHas (name : String) : Int := request Runtime.envHas(name);
export let envSet (name : String, value : String) : Int := request Runtime.envSet(name, value);
export let envRemove (name : String) : Int := request Runtime.envRemove(name);

export let processArgCount () : Int := request Runtime.processArgCount();
export let processArgAt (index : Int) : String := request Runtime.processArgAt(index);
export let processCwd () : String := request Runtime.processCwd();
export let processRun (command : String) : Int := request Runtime.processRun(command);
export let processExit (code : Int) : Unit := request Runtime.processExit(code);

export let timeNowUnixMs () : Int := request Runtime.timeNowUnixMs();
export let timeMonotonicMs () : Int := request Runtime.timeMonotonicMs();
export let timeSleepMs (ms : Int) : Unit := request Runtime.timeSleepMs(ms);
export let randomInt () : Int := request Runtime.randomInt();
export let randomIntInRange (lowerBound : Int, upperBound : Int) : Int :=
  request Runtime.randomIntInRange(lowerBound, upperBound);
export let randomBool () : Int := request Runtime.randomBool();
export let randomFloat01 () : Float := request Runtime.randomFloat01();

export let logInfo (message : String) : Unit := request Runtime.logInfo(message);
export let logWrite (level : Int, message : String) : Unit := request Runtime.logWrite(level, message);
export let ioPrint (text : String) : Unit := request Runtime.ioPrint(text);
export let ioPrintLine (text : String) : Unit := request Runtime.ioPrintLine(text);
export let ioPrintError (text : String) : Unit := request Runtime.ioPrintError(text);
export let ioPrintErrorLine (text : String) : Unit := request Runtime.ioPrintErrorLine(text);
export let ioReadLine () : String := request Runtime.ioReadLine();

export let fsReadText (path : String) : String := request Runtime.fsReadText(path);
export let fsWriteText (path : String, text : String) : Unit :=
  request Runtime.fsWriteText(path, text);
export let fsExists (path : String) : Int := request Runtime.fsExists(path);
export let fsAppendText (path : String, text : String) : Int :=
  request Runtime.fsAppendText(path, text);
export let fsRemove (path : String) : Int := request Runtime.fsRemove(path);
export let fsCreateDirAll (path : String) : Int := request Runtime.fsCreateDirAll(path);

export let textLength (value : String) : Int := request Runtime.textLength(value);
export let textTrim (value : String) : String := request Runtime.textTrim(value);
export let textToLowerCase (value : String) : String := request Runtime.textToLowerCase(value);
export let textToUpperCase (value : String) : String := request Runtime.textToUpperCase(value);
export let textContains (value : String, needle : String) : Int :=
  request Runtime.textContains(value, needle);
export let textStartsWith (value : String, prefix : String) : Int :=
  request Runtime.textStartsWith(value, prefix);
export let textEndsWith (value : String, suffix : String) : Int :=
  request Runtime.textEndsWith(value, suffix);

export let pathJoin (left : String, right : String) : String := request Runtime.pathJoin(left, right);
export let pathNormalize (value : String) : String := request Runtime.pathNormalize(value);
export let pathDirname (value : String) : String := request Runtime.pathDirname(value);
export let pathBasename (value : String) : String := request Runtime.pathBasename(value);
export let pathExtname (value : String) : String := request Runtime.pathExtname(value);
export let pathIsAbsolute (value : String) : Int := request Runtime.pathIsAbsolute(value);

export let jsonIsValid (source : String) : Int := request Runtime.jsonIsValid(source);
export let jsonNormalize (source : String) : String := request Runtime.jsonNormalize(source);

export let hexEncode (source : String) : String := request Runtime.hexEncode(source);
export let hexDecode (source : String) : String := request Runtime.hexDecode(source);
export let hexIsValid (source : String) : Int := request Runtime.hexIsValid(source);

export let base64Encode (source : String) : String := request Runtime.base64Encode(source);
export let base64Decode (source : String) : String := request Runtime.base64Decode(source);
export let base64IsValid (source : String) : Int := request Runtime.base64IsValid(source);

export let utf8Encode (source : String) : String := request Runtime.utf8Encode(source);
export let utf8Decode (source : String) : String := request Runtime.utf8Decode(source);
export let utf8IsValid (source : String) : Int := request Runtime.utf8IsValid(source);
