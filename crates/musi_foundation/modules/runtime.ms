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

export let envGet (name : String) : String := ask Runtime.envGet(name);

export let envHas (name : String) : Int := ask Runtime.envHas(name);

export let envSet (name : String, value : String) : Int :=
  ask Runtime.envSet(name, value);

export let envRemove (name : String) : Int := ask Runtime.envRemove(name);

export let processArgCount () : Int := ask Runtime.processArgCount();

export let processArgAt (index : Int) : String :=
  ask Runtime.processArgAt(index);

export let processCwd () : String := ask Runtime.processCwd();

export let processRun (command : String) : Int :=
  ask Runtime.processRun(command);

export let processExit (code : Int) : Unit := ask Runtime.processExit(code);

export let timeNowUnixMs () : Int := ask Runtime.timeNowUnixMs();

export let timeMonotonicMs () : Int := ask Runtime.timeMonotonicMs();

export let timeSleepMs (ms : Int) : Unit := ask Runtime.timeSleepMs(ms);

export let randomInt () : Int := ask Runtime.randomInt();

export let randomIntInRange (lowerBound : Int, upperBound : Int) : Int
  :=
  ask Runtime.randomIntInRange(lowerBound, upperBound);

export let randomBool () : Int := ask Runtime.randomBool();

export let randomFloat01 () : Float := ask Runtime.randomFloat01();

export let logInfo (message : String) : Unit :=
  ask Runtime.logInfo(message);

export let logWrite (level : Int, message : String) : Unit :=
  ask Runtime.logWrite(level, message);

export let ioPrint (text : String) : Unit := ask Runtime.ioPrint(text);

export let ioPrintLine (text : String) : Unit :=
  ask Runtime.ioPrintLine(text);

export let ioPrintError (text : String) : Unit :=
  ask Runtime.ioPrintError(text);

export let ioPrintErrorLine (text : String) : Unit :=
  ask Runtime.ioPrintErrorLine(text);

export let ioReadLine () : String := ask Runtime.ioReadLine();

export let fsReadText (path : String) : String :=
  ask Runtime.fsReadText(path);

export let fsWriteText (path : String, text : String) : Unit :=
  ask Runtime.fsWriteText (path, text);

export let fsExists (path : String) : Int := ask Runtime.fsExists(path);

export let fsAppendText (path : String, text : String) : Int :=
  ask Runtime.fsAppendText(path, text);

export let fsRemove (path : String) : Int := ask Runtime.fsRemove(path);

export let fsCreateDirAll (path : String) : Int :=
  ask Runtime.fsCreateDirAll(path);

export let textLength (value : String) : Int :=
  ask Runtime.textLength(value);

export let textTrim (value : String) : String :=
  ask Runtime.textTrim (value);

export let textToLowerCase (value : String) : String :=
  ask Runtime.textToLowerCase(value);

export let textToUpperCase (value : String) : String :=
  ask Runtime.textToUpperCase(value);

export let textContains (value : String, needle : String) : Int :=
  ask Runtime.textContains(value, needle);

export let textStartsWith (value : String, prefix : String) : Int :=
  ask Runtime.textStartsWith(value, prefix);

export let textEndsWith (value : String, suffix : String) : Int :=
  ask Runtime.textEndsWith (value, suffix);

export let pathJoin (left : String, right : String) : String :=
  ask Runtime.pathJoin(left, right);

export let pathNormalize (value : String) : String :=
  ask Runtime.pathNormalize(value);

export let pathDirname (value : String) : String :=
  ask Runtime.pathDirname(value);

export let pathBasename (value : String) : String :=
  ask Runtime.pathBasename(value);

export let pathExtname (value : String) : String :=
  ask Runtime.pathExtname(value);

export let pathIsAbsolute (value : String) : Int :=
  ask Runtime.pathIsAbsolute(value);

export let jsonIsValid (source : String) : Int :=
  ask Runtime.jsonIsValid(source);

export let jsonNormalize (source : String) : String :=
  ask Runtime.jsonNormalize(source);

export let hexEncode (source : String) : String :=
  ask Runtime.hexEncode(source);

export let hexDecode (source : String) : String :=
  ask Runtime.hexDecode(source);

export let hexIsValid (source : String) : Int :=
  ask Runtime.hexIsValid(source);

export let base64Encode (source : String) : String :=
  ask Runtime.base64Encode(source);

export let base64Decode (source : String) : String :=
  ask Runtime.base64Decode(source);

export let base64IsValid (source : String) : Int :=
  ask Runtime.base64IsValid(source);

export let utf8Encode (source : String) : String :=
  ask Runtime.utf8Encode(source);

export let utf8Decode (source : String) : String :=
  ask Runtime.utf8Decode(source);

export let utf8IsValid (source : String) : Int :=
  ask Runtime.utf8IsValid(source);
