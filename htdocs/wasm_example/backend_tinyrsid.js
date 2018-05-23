// create separate namespace for all the Emscripten stuff.. otherwise naming clashes may occur especially when 
// optimizing using closure compiler..
window.spp_backend_state_SID= {
	notReady: true,
	adapterCallback: function(){}	// overwritten later	
};
window.spp_backend_state_SID["onRuntimeInitialized"] = function() {	// emscripten callback needed in case async init is used (e.g. for WASM)
	this.notReady= false;
	this.adapterCallback();
}.bind(window.spp_backend_state_SID);

var backend_SID = (function(Module) {
var a;a||(a=typeof Module !== 'undefined' ? Module : {});var e={},f;for(f in a)a.hasOwnProperty(f)&&(e[f]=a[f]);a.arguments=[];a.thisProgram="./this.program";a.quit=function(b,c){throw c;};a.preRun=[];a.postRun=[];var g=!1,h=!1,p=!1,q=!1;
if(a.ENVIRONMENT)if("WEB"===a.ENVIRONMENT)g=!0;else if("WORKER"===a.ENVIRONMENT)h=!0;else if("NODE"===a.ENVIRONMENT)p=!0;else if("SHELL"===a.ENVIRONMENT)q=!0;else throw Error("Module['ENVIRONMENT'] value is not valid. must be one of: WEB|WORKER|NODE|SHELL.");else g="object"===typeof window,h="function"===typeof importScripts,p="object"===typeof process&&"function"===typeof require&&!g&&!h,q=!g&&!p&&!h;
if(p){var t,u;a.read=function(b,c){t||(t=require("fs"));u||(u=require("path"));b=u.normalize(b);b=t.readFileSync(b);return c?b:b.toString()};a.readBinary=function(b){b=a.read(b,!0);b.buffer||(b=new Uint8Array(b));assert(b.buffer);return b};1<process.argv.length&&(a.thisProgram=process.argv[1].replace(/\\/g,"/"));a.arguments=process.argv.slice(2);"undefined"!==typeof module&&(module.exports=a);process.on("uncaughtException",function(b){if(!(b instanceof v))throw b;});process.on("unhandledRejection",
function(){a.printErr("node.js exiting due to unhandled promise rejection");process.exit(1)});a.inspect=function(){return"[Emscripten Module object]"}}else if(q)"undefined"!=typeof read&&(a.read=function(b){return read(b)}),a.readBinary=function(b){if("function"===typeof readbuffer)return new Uint8Array(readbuffer(b));b=read(b,"binary");assert("object"===typeof b);return b},"undefined"!=typeof scriptArgs?a.arguments=scriptArgs:"undefined"!=typeof arguments&&(a.arguments=arguments),"function"===typeof quit&&
(a.quit=function(b){quit(b)});else if(g||h)a.read=function(b){var c=new XMLHttpRequest;c.open("GET",b,!1);c.send(null);return c.responseText},h&&(a.readBinary=function(b){var c=new XMLHttpRequest;c.open("GET",b,!1);c.responseType="arraybuffer";c.send(null);return new Uint8Array(c.response)}),a.readAsync=function(b,c,k){var d=new XMLHttpRequest;d.open("GET",b,!0);d.responseType="arraybuffer";d.onload=function(){200==d.status||0==d.status&&d.response?c(d.response):k()};d.onerror=k;d.send(null)},"undefined"!=
typeof arguments&&(a.arguments=arguments),a.setWindowTitle=function(b){document.title=b};else throw Error("unknown runtime environment");a.print="undefined"!==typeof console?console.log:"undefined"!==typeof print?print:null;a.printErr="undefined"!==typeof printErr?printErr:"undefined"!==typeof console&&console.warn||a.print;a.print=a.print;a.printErr=a.printErr;for(f in e)e.hasOwnProperty(f)&&(a[f]=e[f]);e=void 0;w=x=y=function(){z("cannot use the stack before compiled code is ready to run, and has provided stack access")};
function aa(b){var c;c||(c=16);return Math.ceil(b/c)*c}function ba(b){A||(A={});A[b]||(A[b]=1,a.printErr(b))}var A,B=0;function assert(b,c){b||z("Assertion failed: "+c)}
var da={stackSave:function(){w()},stackRestore:function(){x()},arrayToC:function(b){var c=y(b.length);assert(0<=b.length,"writeArrayToMemory array must have a length (should be an array or typed array)");ca.set(b,c);return c},stringToC:function(b){var c=0;if(null!==b&&void 0!==b&&0!==b){var k=(b.length<<2)+1,d=c=y(k);assert("number"==typeof k,"stringToUTF8(str, outPtr, maxBytesToWrite) is missing the third parameter that specifies the length of the output buffer!");var m=C;if(0<k){k=d+k-1;for(var n=
0;n<b.length;++n){var l=b.charCodeAt(n);55296<=l&&57343>=l&&(l=65536+((l&1023)<<10)|b.charCodeAt(++n)&1023);if(127>=l){if(d>=k)break;m[d++]=l}else{if(2047>=l){if(d+1>=k)break;m[d++]=192|l>>6}else{if(65535>=l){if(d+2>=k)break;m[d++]=224|l>>12}else{if(2097151>=l){if(d+3>=k)break;m[d++]=240|l>>18}else{if(67108863>=l){if(d+4>=k)break;m[d++]=248|l>>24}else{if(d+5>=k)break;m[d++]=252|l>>30;m[d++]=128|l>>24&63}m[d++]=128|l>>18&63}m[d++]=128|l>>12&63}m[d++]=128|l>>6&63}m[d++]=128|l&63}}m[d]=0}}return c}},
ea={string:da.stringToC,array:da.arrayToC};
function fa(b){var c;if(0===c||!b)return"";for(var k=0,d,m=0;;){assert(b+m<D);d=C[b+m>>0];k|=d;if(0==d&&!c)break;m++;if(c&&m==c)break}c||(c=m);d="";if(128>k){for(;0<c;)k=String.fromCharCode.apply(String,C.subarray(b,b+Math.min(c,1024))),d=d?d+k:k,b+=1024,c-=1024;return d}a:{c=C;for(k=b;c[k];)++k;if(16<k-b&&c.subarray&&ha)b=ha.decode(c.subarray(b,k));else for(k="";;){d=c[b++];if(!d){b=k;break a}if(d&128)if(m=c[b++]&63,192==(d&224))k+=String.fromCharCode((d&31)<<6|m);else{var n=c[b++]&63;if(224==(d&
240))d=(d&15)<<12|m<<6|n;else{var l=c[b++]&63;if(240==(d&248))d=(d&7)<<18|m<<12|n<<6|l;else{var r=c[b++]&63;if(248==(d&252))d=(d&3)<<24|m<<18|n<<12|l<<6|r;else{var Y=c[b++]&63;d=(d&1)<<30|m<<24|n<<18|l<<12|r<<6|Y}}}65536>d?k+=String.fromCharCode(d):(d-=65536,k+=String.fromCharCode(55296|d>>10,56320|d&1023))}else k+=String.fromCharCode(d)}}return b}var ha="undefined"!==typeof TextDecoder?new TextDecoder("utf8"):void 0;"undefined"!==typeof TextDecoder&&new TextDecoder("utf-16le");
function ia(b){return b.replace(/__Z[\w\d_]+/g,function(b){ba("warning: build with  -s DEMANGLE_SUPPORT=1  to link in libcxxabi demangling");return b===b?b:b+" ["+b+"]"})}function ja(){a:{var b=Error();if(!b.stack){try{throw Error(0);}catch(c){b=c}if(!b.stack){b="(no stack trace available)";break a}}b=b.stack.toString()}a.extraStackTrace&&(b+="\n"+a.extraStackTrace());return ia(b)}var buffer,ca,C,ka,E,F;
function la(){a.HEAP8=ca=new Int8Array(buffer);a.HEAP16=ka=new Int16Array(buffer);a.HEAP32=E=new Int32Array(buffer);a.HEAPU8=C=new Uint8Array(buffer);a.HEAPU16=new Uint16Array(buffer);a.HEAPU32=F=new Uint32Array(buffer);a.HEAPF32=new Float32Array(buffer);a.HEAPF64=new Float64Array(buffer)}var G,H,I,ma,J,K,L,M;G=H=ma=J=K=L=M=0;I=!1;
function N(){34821223==F[(K>>2)-1]&&2310721022==F[(K>>2)-2]||z("Stack overflow! Stack cookie has been overwritten, expected hex dwords 0x89BACDFE and 0x02135467, but received 0x"+F[(K>>2)-2].toString(16)+" "+F[(K>>2)-1].toString(16));if(1668509029!==E[0])throw"Runtime error: The application has corrupted its heap memory area (address zero)!";}
function na(){z("Cannot enlarge memory arrays. Either (1) compile with  -s TOTAL_MEMORY=X  with X higher than the current value "+D+", (2) compile with  -s ALLOW_MEMORY_GROWTH=1  which allows increasing the size at runtime, or (3) if you want malloc to return NULL (0) instead of this abort, compile with  -s ABORTING_MALLOC=0 ")}var oa=a.TOTAL_STACK||5242880,D=a.TOTAL_MEMORY||16777216;D<oa&&a.printErr("TOTAL_MEMORY should be larger than TOTAL_STACK, was "+D+"! (TOTAL_STACK="+oa+")");
assert("undefined"!==typeof Int32Array&&"undefined"!==typeof Float64Array&&void 0!==Int32Array.prototype.subarray&&void 0!==Int32Array.prototype.set,"JS engine does not provide full typed array support");
a.buffer?(buffer=a.buffer,assert(buffer.byteLength===D,"provided buffer should be "+D+" bytes, but it is "+buffer.byteLength)):("object"===typeof WebAssembly&&"function"===typeof WebAssembly.Memory?(assert(0===D%65536),a.wasmMemory=new WebAssembly.Memory({initial:D/65536,maximum:D/65536}),buffer=a.wasmMemory.buffer):buffer=new ArrayBuffer(D),assert(buffer.byteLength===D),a.buffer=buffer);la();E[0]=1668509029;ka[1]=25459;
if(115!==C[2]||99!==C[3])throw"Runtime error: expected the system to be little-endian!";function O(b){for(;0<b.length;){var c=b.shift();if("function"==typeof c)c();else{var k=c.f;"number"===typeof k?void 0===c.a?a.dynCall_v(k):a.dynCall_vi(k,c.a):k(void 0===c.a?null:c.a)}}}var pa=[],qa=[],ra=[],sa=[],ta=[],P=!1,Q=!1;function ua(){var b=a.preRun.shift();pa.unshift(b)}assert(Math.imul&&Math.fround&&Math.clz32&&Math.trunc,"this is a legacy browser, build with LEGACY_VM_SUPPORT");
var S=0,T=null,U=null,V={};function va(){S++;a.monitorRunDependencies&&a.monitorRunDependencies(S);assert(!V["wasm-instantiate"]);V["wasm-instantiate"]=1;null===T&&"undefined"!==typeof setInterval&&(T=setInterval(function(){if(B)clearInterval(T),T=null;else{var b=!1,c;for(c in V)b||(b=!0,a.printErr("still waiting on run dependencies:")),a.printErr("dependency: "+c);b&&a.printErr("(end of list)")}},1E4))}a.preloadedImages={};a.preloadedAudios={};
function wa(){z("Filesystem support (FS) was not included. The problem is that you are using files from JS, but files were not used from C/C++, so filesystem support was not auto-included. You can force-include filesystem support with  -s FORCE_FILESYSTEM=1")}a.FS_createDataFile=function(){wa()};a.FS_createPreloadedFile=function(){wa()};function W(b){return String.prototype.startsWith?b.startsWith("data:application/octet-stream;base64,"):0===b.indexOf("data:application/octet-stream;base64,")}
(function(){function b(){try{if(a.wasmBinary)return new Uint8Array(a.wasmBinary);if(a.readBinary)return a.readBinary(m);throw"on the web, we need the wasm binary to be preloaded and set on Module['wasmBinary']. emcc.py will do that for you when generating HTML (but not JS)";}catch(R){z(R)}}function c(){return a.wasmBinary||!g&&!h||"function"!==typeof fetch?new Promise(function(c){c(b())}):fetch(m,{credentials:"same-origin"}).then(function(b){if(!b.ok)throw"failed to load wasm binary file at '"+m+
"'";return b.arrayBuffer()}).catch(function(){return b()})}function k(b){function d(b){r=b.exports;if(r.memory){b=r.memory;var c=a.buffer;b.byteLength<c.byteLength&&a.printErr("the new buffer in mergeMemory is smaller than the previous one. in native wasm, we should grow memory here");c=new Int8Array(c);(new Int8Array(b)).set(c);a.buffer=buffer=b;la()}a.asm=r;a.usingWasm=!0;S--;a.monitorRunDependencies&&a.monitorRunDependencies(S);assert(V["wasm-instantiate"]);delete V["wasm-instantiate"];0==S&&(null!==
T&&(clearInterval(T),T=null),U&&(b=U,U=null,b()))}function k(b){assert(a===R,"the Module object should not be replaced during async compilation - perhaps the order of HTML elements is wrong?");R=null;d(b.instance)}function n(b){c().then(function(b){return WebAssembly.instantiate(b,l)}).then(b).catch(function(b){a.printErr("failed to asynchronously prepare wasm: "+b);z(b)})}if("object"!==typeof WebAssembly)return a.printErr("no native wasm support detected"),!1;if(!(a.wasmMemory instanceof WebAssembly.Memory))return a.printErr("no native wasm Memory in use"),
!1;b.memory=a.wasmMemory;l.global={NaN:NaN,Infinity:Infinity};l["global.Math"]=Math;l.env=b;va();if(a.instantiateWasm)try{return a.instantiateWasm(l,d)}catch(za){return a.printErr("Module.instantiateWasm callback failed with error: "+za),!1}var R=a;a.wasmBinary||"function"!==typeof WebAssembly.instantiateStreaming||W(m)||"function"!==typeof fetch?n(k):WebAssembly.instantiateStreaming(fetch(m,{credentials:"same-origin"}),l).then(k).catch(function(b){a.printErr("wasm streaming compile failed: "+b);
a.printErr("falling back to ArrayBuffer instantiation");n(k)});return{}}var d="tinyrsid.wast",m="tinyrsid.wasm",n="tinyrsid.temp.asm.js";"function"===typeof a.locateFile&&(W(d)||(d=a.locateFile(d)),W(m)||(m=a.locateFile(m)),W(n)||(n=a.locateFile(n)));var l={global:null,env:null,asm2wasm:{"f64-rem":function(b,c){return b%c},"debugger":function(){debugger}},parent:a},r=null;a.asmPreload=a.asm;var Y=a.reallocBuffer;a.reallocBuffer=function(b){if("asmjs"===Ba)var c=Y(b);else a:{var d=a.usingWasm?65536:
16777216;0<b%d&&(b+=d-b%d);d=a.buffer.byteLength;if(a.usingWasm)try{c=-1!==a.wasmMemory.grow((b-d)/65536)?a.buffer=a.wasmMemory.buffer:null;break a}catch(Aa){console.error("Module.reallocBuffer: Attempted to grow from "+d+" bytes to "+b+" bytes, but got error: "+Aa);c=null;break a}c=void 0}return c};var Ba="";a.asm=function(b,c){if(!c.table){b=a.wasmTableSize;void 0===b&&(b=1024);var d=a.wasmMaxTableSize;c.table="object"===typeof WebAssembly&&"function"===typeof WebAssembly.Table?void 0!==d?new WebAssembly.Table({initial:b,
maximum:d,element:"anyfunc"}):new WebAssembly.Table({initial:b,element:"anyfunc"}):Array(b);a.wasmTable=c.table}c.memoryBase||(c.memoryBase=a.STATIC_BASE);c.tableBase||(c.tableBase=0);(c=k(c))||z("no binaryen method succeeded. consider enabling more options, like interpreting, if you want that: https://github.com/kripken/emscripten/wiki/WebAssembly#binaryen-methods");return c}})();G=1024;H=G+338064;qa.push();a.STATIC_BASE=G;a.STATIC_BUMP=338064;var xa=H;H+=16;assert(0==xa%8);
function ya(b){return Math.pow(2,b)}assert(!I);var Ca=H;H=H+4+15&-16;M=Ca;ma=J=aa(H);K=ma+oa;L=aa(K);E[M>>2]=L;I=!0;assert(L<D,"TOTAL_MEMORY not big enough for stack");a.wasmTableSize=0;a.wasmMaxTableSize=0;a.b={};
a.c={enlargeMemory:function(){na()},getTotalMemory:function(){return D},abortOnCannotGrowMemory:na,abortStackOverflow:function(b){z("Stack overflow! Attempted to allocate "+b+" bytes on the stack, but stack has only "+(K-w()+b)+" bytes available!")},___setErrNo:function(b){a.___errno_location?E[a.___errno_location()>>2]=b:a.printErr("failed to set errno from JS");return b},_emscripten_memcpy_big:function(b,c,k){C.set(C.subarray(c,c+k),b);return b},_llvm_exp2_f64:function(){return ya.apply(null,arguments)},
DYNAMICTOP_PTR:M,STACKTOP:J,STACK_MAX:K};var X=a.asm(a.b,a.c,buffer),Da=X._computeAudioSamples;X._computeAudioSamples=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Da.apply(null,arguments)};var Ea=X._enableVoices;
X._enableVoices=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Ea.apply(null,arguments)};var Fa=X._envIsNTSC;X._envIsNTSC=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Fa.apply(null,arguments)};
var Ga=X._envIsSID6581;X._envIsSID6581=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Ga.apply(null,arguments)};var Ha=X._envSetNTSC;
X._envSetNTSC=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Ha.apply(null,arguments)};var Ia=X._envSetSID6581;
X._envSetSID6581=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Ia.apply(null,arguments)};var Ja=X._free;X._free=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Ja.apply(null,arguments)};
var Ka=X._getBufferVoice1;X._getBufferVoice1=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Ka.apply(null,arguments)};var La=X._getBufferVoice2;
X._getBufferVoice2=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return La.apply(null,arguments)};var Ma=X._getBufferVoice3;
X._getBufferVoice3=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Ma.apply(null,arguments)};var Na=X._getBufferVoice4;
X._getBufferVoice4=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Na.apply(null,arguments)};var Oa=X._getMusicInfo;
X._getMusicInfo=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Oa.apply(null,arguments)};var Pa=X._getRegisterSID;
X._getRegisterSID=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Pa.apply(null,arguments)};var Qa=X._getSampleRate;
X._getSampleRate=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Qa.apply(null,arguments)};var Ra=X._getSoundBuffer;
X._getSoundBuffer=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Ra.apply(null,arguments)};var Sa=X._getSoundBufferLen;
X._getSoundBufferLen=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Sa.apply(null,arguments)};var Ta=X._loadSidFile;
X._loadSidFile=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Ta.apply(null,arguments)};var Ua=X._malloc;X._malloc=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Ua.apply(null,arguments)};
var Va=X._playTune;X._playTune=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Va.apply(null,arguments)};var Wa=X._round;
X._round=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Wa.apply(null,arguments)};var Xa=X._sbrk;X._sbrk=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Xa.apply(null,arguments)};
var Ya=X.establishStackSpace;X.establishStackSpace=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Ya.apply(null,arguments)};var Za=X.getTempRet0;
X.getTempRet0=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return Za.apply(null,arguments)};var $a=X.setTempRet0;X.setTempRet0=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return $a.apply(null,arguments)};
var ab=X.setThrew;X.setThrew=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return ab.apply(null,arguments)};var bb=X.stackAlloc;
X.stackAlloc=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return bb.apply(null,arguments)};var cb=X.stackRestore;X.stackRestore=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return cb.apply(null,arguments)};
var db=X.stackSave;X.stackSave=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return db.apply(null,arguments)};a.asm=X;
a._computeAudioSamples=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._computeAudioSamples.apply(null,arguments)};
a._enableVoices=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._enableVoices.apply(null,arguments)};
a._envIsNTSC=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._envIsNTSC.apply(null,arguments)};
a._envIsSID6581=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._envIsSID6581.apply(null,arguments)};
a._envSetNTSC=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._envSetNTSC.apply(null,arguments)};
a._envSetSID6581=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._envSetSID6581.apply(null,arguments)};
a._free=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._free.apply(null,arguments)};
a._getBufferVoice1=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._getBufferVoice1.apply(null,arguments)};
a._getBufferVoice2=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._getBufferVoice2.apply(null,arguments)};
a._getBufferVoice3=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._getBufferVoice3.apply(null,arguments)};
a._getBufferVoice4=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._getBufferVoice4.apply(null,arguments)};
a._getMusicInfo=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._getMusicInfo.apply(null,arguments)};
a._getRegisterSID=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._getRegisterSID.apply(null,arguments)};
a._getSampleRate=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._getSampleRate.apply(null,arguments)};
a._getSoundBuffer=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._getSoundBuffer.apply(null,arguments)};
a._getSoundBufferLen=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._getSoundBufferLen.apply(null,arguments)};
a._loadSidFile=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._loadSidFile.apply(null,arguments)};
a._malloc=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._malloc.apply(null,arguments)};a._playTune=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._playTune.apply(null,arguments)};
a._round=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._round.apply(null,arguments)};a._sbrk=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm._sbrk.apply(null,arguments)};
a.establishStackSpace=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm.establishStackSpace.apply(null,arguments)};
a.getTempRet0=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm.getTempRet0.apply(null,arguments)};
a.setTempRet0=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm.setTempRet0.apply(null,arguments)};
a.setThrew=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm.setThrew.apply(null,arguments)};
var y=a.stackAlloc=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm.stackAlloc.apply(null,arguments)},x=a.stackRestore=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm.stackRestore.apply(null,
arguments)},w=a.stackSave=function(){assert(P,"you need to wait for the runtime to be ready (e.g. wait for main() to be called)");assert(!Q,"the runtime was exited (use NO_EXIT_RUNTIME to keep it alive after main() exits)");return a.asm.stackSave.apply(null,arguments)};a.asm=X;a.intArrayFromString||(a.intArrayFromString=function(){z("'intArrayFromString' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.intArrayToString||(a.intArrayToString=function(){z("'intArrayToString' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});
a.ccall=function(b,c,k,d){var m=a["_"+b];assert(m,"Cannot call unknown function "+b+", make sure it is exported");var n=[];b=0;assert("array"!==c,'Return type should not be "array".');if(d)for(var l=0;l<d.length;l++){var r=ea[k[l]];r?(0===b&&(b=w()),n[l]=r(d[l])):n[l]=d[l]}k=m.apply(null,n);"string"===c&&(k=fa(k));0!==b&&x(b);return k};a.cwrap||(a.cwrap=function(){z("'cwrap' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.setValue||(a.setValue=function(){z("'setValue' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});
a.getValue||(a.getValue=function(){z("'getValue' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.allocate||(a.allocate=function(){z("'allocate' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.getMemory||(a.getMemory=function(){z("'getMemory' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ). Alternatively, forcing filesystem support (-s FORCE_FILESYSTEM=1) can export this for you")});
a.Pointer_stringify||(a.Pointer_stringify=function(){z("'Pointer_stringify' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.AsciiToString||(a.AsciiToString=function(){z("'AsciiToString' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.stringToAscii||(a.stringToAscii=function(){z("'stringToAscii' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.UTF8ArrayToString||(a.UTF8ArrayToString=function(){z("'UTF8ArrayToString' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});
a.UTF8ToString||(a.UTF8ToString=function(){z("'UTF8ToString' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.stringToUTF8Array||(a.stringToUTF8Array=function(){z("'stringToUTF8Array' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.stringToUTF8||(a.stringToUTF8=function(){z("'stringToUTF8' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.UTF16ToString||(a.UTF16ToString=function(){z("'UTF16ToString' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});
a.stringToUTF16||(a.stringToUTF16=function(){z("'stringToUTF16' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.lengthBytesUTF16||(a.lengthBytesUTF16=function(){z("'lengthBytesUTF16' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.UTF32ToString||(a.UTF32ToString=function(){z("'UTF32ToString' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.stringToUTF32||(a.stringToUTF32=function(){z("'stringToUTF32' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});
a.lengthBytesUTF32||(a.lengthBytesUTF32=function(){z("'lengthBytesUTF32' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.allocateUTF8||(a.allocateUTF8=function(){z("'allocateUTF8' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.stackTrace||(a.stackTrace=function(){z("'stackTrace' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.addOnPreRun||(a.addOnPreRun=function(){z("'addOnPreRun' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});
a.addOnInit||(a.addOnInit=function(){z("'addOnInit' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.addOnPreMain||(a.addOnPreMain=function(){z("'addOnPreMain' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.addOnExit||(a.addOnExit=function(){z("'addOnExit' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.addOnPostRun||(a.addOnPostRun=function(){z("'addOnPostRun' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});
a.writeStringToMemory||(a.writeStringToMemory=function(){z("'writeStringToMemory' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.writeArrayToMemory||(a.writeArrayToMemory=function(){z("'writeArrayToMemory' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.writeAsciiToMemory||(a.writeAsciiToMemory=function(){z("'writeAsciiToMemory' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});
a.addRunDependency||(a.addRunDependency=function(){z("'addRunDependency' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ). Alternatively, forcing filesystem support (-s FORCE_FILESYSTEM=1) can export this for you")});a.removeRunDependency||(a.removeRunDependency=function(){z("'removeRunDependency' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ). Alternatively, forcing filesystem support (-s FORCE_FILESYSTEM=1) can export this for you")});
a.FS||(a.FS=function(){z("'FS' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.FS_createFolder||(a.FS_createFolder=function(){z("'FS_createFolder' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ). Alternatively, forcing filesystem support (-s FORCE_FILESYSTEM=1) can export this for you")});a.FS_createPath||(a.FS_createPath=function(){z("'FS_createPath' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ). Alternatively, forcing filesystem support (-s FORCE_FILESYSTEM=1) can export this for you")});
a.FS_createDataFile||(a.FS_createDataFile=function(){z("'FS_createDataFile' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ). Alternatively, forcing filesystem support (-s FORCE_FILESYSTEM=1) can export this for you")});a.FS_createPreloadedFile||(a.FS_createPreloadedFile=function(){z("'FS_createPreloadedFile' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ). Alternatively, forcing filesystem support (-s FORCE_FILESYSTEM=1) can export this for you")});
a.FS_createLazyFile||(a.FS_createLazyFile=function(){z("'FS_createLazyFile' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ). Alternatively, forcing filesystem support (-s FORCE_FILESYSTEM=1) can export this for you")});a.FS_createLink||(a.FS_createLink=function(){z("'FS_createLink' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ). Alternatively, forcing filesystem support (-s FORCE_FILESYSTEM=1) can export this for you")});
a.FS_createDevice||(a.FS_createDevice=function(){z("'FS_createDevice' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ). Alternatively, forcing filesystem support (-s FORCE_FILESYSTEM=1) can export this for you")});a.FS_unlink||(a.FS_unlink=function(){z("'FS_unlink' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ). Alternatively, forcing filesystem support (-s FORCE_FILESYSTEM=1) can export this for you")});a.GL||(a.GL=function(){z("'GL' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});
a.staticAlloc||(a.staticAlloc=function(){z("'staticAlloc' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.dynamicAlloc||(a.dynamicAlloc=function(){z("'dynamicAlloc' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.warnOnce||(a.warnOnce=function(){z("'warnOnce' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.loadDynamicLibrary||(a.loadDynamicLibrary=function(){z("'loadDynamicLibrary' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});
a.loadWebAssemblyModule||(a.loadWebAssemblyModule=function(){z("'loadWebAssemblyModule' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.getLEB||(a.getLEB=function(){z("'getLEB' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.getFunctionTables||(a.getFunctionTables=function(){z("'getFunctionTables' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.alignFunctionTables||(a.alignFunctionTables=function(){z("'alignFunctionTables' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});
a.registerFunctions||(a.registerFunctions=function(){z("'registerFunctions' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.addFunction||(a.addFunction=function(){z("'addFunction' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.removeFunction||(a.removeFunction=function(){z("'removeFunction' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.getFuncWrapper||(a.getFuncWrapper=function(){z("'getFuncWrapper' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});
a.prettyPrint||(a.prettyPrint=function(){z("'prettyPrint' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.makeBigInt||(a.makeBigInt=function(){z("'makeBigInt' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.dynCall||(a.dynCall=function(){z("'dynCall' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});a.getCompilerSetting||(a.getCompilerSetting=function(){z("'getCompilerSetting' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")});
a.ALLOC_NORMAL||Object.defineProperty(a,"ALLOC_NORMAL",{get:function(){z("'ALLOC_NORMAL' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")}});a.ALLOC_STACK||Object.defineProperty(a,"ALLOC_STACK",{get:function(){z("'ALLOC_STACK' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")}});a.ALLOC_STATIC||Object.defineProperty(a,"ALLOC_STATIC",{get:function(){z("'ALLOC_STATIC' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")}});
a.ALLOC_DYNAMIC||Object.defineProperty(a,"ALLOC_DYNAMIC",{get:function(){z("'ALLOC_DYNAMIC' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")}});a.ALLOC_NONE||Object.defineProperty(a,"ALLOC_NONE",{get:function(){z("'ALLOC_NONE' was not exported. add it to EXTRA_EXPORTED_RUNTIME_METHODS (see the FAQ)")}});function v(b){this.name="ExitStatus";this.message="Program terminated with exit("+b+")";this.status=b}v.prototype=Error();v.prototype.constructor=v;var Z=null;
U=function eb(){a.calledRun||fb();a.calledRun||(U=eb)};
function fb(){function b(){if(!a.calledRun&&(a.calledRun=!0,!B)){N();P||(P=!0,O(qa));N();O(ra);g&&null!==Z&&a.printErr("pre-main prep time: "+(Date.now()-Z)+" ms");if(a.onRuntimeInitialized)a.onRuntimeInitialized();assert(!a._main,'compiled without a main, but one is present. if you added it from JS, use Module["onRuntimeInitialized"]');N();if(a.postRun)for("function"==typeof a.postRun&&(a.postRun=[a.postRun]);a.postRun.length;){var b=a.postRun.shift();ta.unshift(b)}O(ta)}}null===Z&&(Z=Date.now());
if(!(0<S)){assert(0==(K&3));F[(K>>2)-1]=34821223;F[(K>>2)-2]=2310721022;if(a.preRun)for("function"==typeof a.preRun&&(a.preRun=[a.preRun]);a.preRun.length;)ua();O(pa);0<S||a.calledRun||(a.setStatus?(a.setStatus("Running..."),setTimeout(function(){setTimeout(function(){a.setStatus("")},1);b()},1)):b(),N())}}a.run=fb;
function gb(){var b=a.print,c=a.printErr,k=!1;a.print=a.printErr=function(){k=!0};a.print=b;a.printErr=c;k&&ba("stdio streams had content in them that was not flushed. you should set NO_EXIT_RUNTIME to 0 (see the FAQ), or make sure to emit a newline when you printf etc.")}
a.exit=function(b,c){gb();if(!c||!a.noExitRuntime||0!==b){if(a.noExitRuntime)c||a.printErr("exit("+b+") called, but NO_EXIT_RUNTIME is set, so halting execution but not exiting the runtime or preventing further async execution (build with NO_EXIT_RUNTIME=0, if you want a true shutdown)");else if(B=!0,J=void 0,N(),O(sa),Q=!0,a.onExit)a.onExit(b);p&&process.exit(b);a.quit(b,new v(b))}};var hb=[];
function z(b){if(a.onAbort)a.onAbort(b);void 0!==b?(a.print(b),a.printErr(b),b=JSON.stringify(b)):b="";B=!0;var c="abort("+b+") at "+ja()+"";hb&&hb.forEach(function(k){c=k(c,b)});throw c;}a.abort=z;if(a.preInit)for("function"==typeof a.preInit&&(a.preInit=[a.preInit]);0<a.preInit.length;)a.preInit.pop()();a.noExitRuntime=!0;fb();
  return {
	Module: Module,  // expose original Module
  };
})(window.spp_backend_state_SID);
/*
 tinyrsid_adapter.js: Adapts Tiny'R'Sid backend to generic WebAudio/ScriptProcessor player.
 
 version 1.0
 
 	Copyright (C) 2015 Juergen Wothke

 LICENSE
 
 This library is free software; you can redistribute it and/or modify it
 under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2.1 of the License, or (at
 your option) any later version. This library is distributed in the hope
 that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
*/
SIDBackendAdapter = (function(){ var $this = function () { 
		$this.base.call(this, backend_SID.Module, 1);
		this.playerSampleRate;
	}; 
	// TinyRSid's sample buffer contains 2-byte (signed short) sample data 
	// for 1 channel
	extend(EmsHEAP16BackendAdapter, $this, {
		getAudioBuffer: function() {
			var ptr=  this.Module.ccall('getSoundBuffer', 'number');			
			return ptr>>1;	// 16 bit samples			
		},
		getAudioBufferLength: function() {
			var len= this.Module.ccall('getSoundBufferLen', 'number');
			return len;
		},
		computeAudioSamples: function() {
			var len= this.Module.ccall('computeAudioSamples', 'number');
			if (len <= 0) return 1; // >0 means "end song"
			
			return 0;	
		},
		getPathAndFilename: function(filename) {
			return ['/', filename];
		},
		registerFileData: function(pathFilenameArray, data) {
			return 0;	// FS not used in Tiny'R'Sid
		},
		loadMusicData: function(sampleRate, path, filename, data, options) {
			var buf = this.Module._malloc(data.length);
			this.Module.HEAPU8.set(data, buf);
			
			var isMus= filename.endsWith(".mus") || filename.endsWith(".str");	// Compute! Sidplayer file (stereo files not supported)			
			var ret = this.Module.ccall('loadSidFile', 'number', ['number', 'number', 'number'], [isMus, buf, data.length]);
			this.Module._free(buf);

			if (ret == 0) {
				this.playerSampleRate = this.Module.ccall('getSampleRate', 'number');
				this.resetSampleRate(sampleRate, this.playerSampleRate); 
			}
			return ret;			
		},
		evalTrackOptions: function(options) {
			if (typeof options.timeout != 'undefined') {
				ScriptNodePlayer.getInstance().setPlaybackTimeout(options.timeout*1000);
			}
			var traceSID= false;
			if (typeof options.traceSID != 'undefined') {
				traceSID= options.traceSID;
			}
			return this.Module.ccall('playTune', 'number', ['number', 'number'], [options.track, traceSID]);
		},
		teardown: function() {
			// nothing to do
		},
		getSongInfoMeta: function() {
			return {			
					loadAddr: Number,
					playSpeed: Number,
					maxSubsong: Number,
					actualSubsong: Number,
					songName: String,
					songAuthor: String, 
					songReleased: String 
					};
		},
		getExtAsciiString: function(heapPtr) {
			// Pointer_stringify cannot be used here since UTF-8 parsing 
			// messes up original extASCII content
			var text="";
			for (var j= 0; j<32; j++) {
				var b= this.Module.HEAP8[heapPtr+j] & 0xff;
				if(b ==0) break;
				
				if(b < 128){
					text = text + String.fromCharCode(b);
				} else {
					text = text + "&#" + b + ";";
				}
			}
			return text;
		},
		updateSongInfo: function(filename, result) {
		// get song infos (so far only use some top level module infos)
			var numAttr= 7;
			var ret = this.Module.ccall('getMusicInfo', 'number');
						
			var array = this.Module.HEAP32.subarray(ret>>2, (ret>>2)+7);
			result.loadAddr= this.Module.HEAP32[((array[0])>>2)]; // i32
			result.playSpeed= this.Module.HEAP32[((array[1])>>2)]; // i32
			result.maxSubsong= this.Module.HEAP8[(array[2])]; // i8
			result.actualSubsong= this.Module.HEAP8[(array[3])]; // i8
			result.songName= this.getExtAsciiString(array[4]);			
			result.songAuthor= this.getExtAsciiString(array[5]);
			result.songReleased= this.getExtAsciiString(array[6]);			
		},
		// for debugging.. disable voices (0-2) by clearing respective bit
		enableVoices: function(mask) {
			this.Module.ccall('enableVoices', 'number', ['number'], [mask]);
		},
		
		// C64 emu specific accessors (that might be useful in GUI)
		isSID6581: function() {
			return this.Module.ccall('envIsSID6581', 'number');
		},
		setSID6581: function(is6581) {
			this.Module.ccall('envSetSID6581', 'number', ['number'], [is6581]);
		},
		isNTSC: function() {
			return this.Module.ccall('envIsNTSC', 'number');
		},
		setNTSC: function(ntsc) {
			this.Module.ccall('envSetNTSC', 'number', ['number'], [ntsc]);
		},
		
		// To activate the below output a song must be started with the "traceSID" option set to 1:
		// At any given moment the below getters will then correspond to the output of getAudioBuffer
		// and what has last been generated by computeAudioSamples. They expose some of the respective
		// underlying internal SID state (the "filter" is NOT reflected in this data).
		getBufferVoice1: function() {
			var ptr=  this.Module.ccall('getBufferVoice1', 'number');			
			return ptr>>1;	// 16 bit samples			
		},
		getBufferVoice2: function() {
			var ptr=  this.Module.ccall('getBufferVoice2', 'number');			
			return ptr>>1;	// 16 bit samples			
		},
		getBufferVoice3: function() {
			var ptr=  this.Module.ccall('getBufferVoice3', 'number');			
			return ptr>>1;	// 16 bit samples			
		},
		getBufferVoice4: function() {
			var ptr=  this.Module.ccall('getBufferVoice4', 'number');			
			return ptr>>1;	// 16 bit samples			
		},		
		/**
		* This just queries the *current* state of the emulator. It
		* is less precisely correlated to the music that is currently playing (than the above
		* buffers), i.e. it represents the state *after* the last emulator call (respective data
		* may not yet have been fed to WebAudio or if it has already been fed then 
		* WebAudio may not yet be playing it yet). The lag should normally not be very large 
		* (<0.2s) and when using it for display purposes it would be hard to see a difference anyway.
		*/
		getRegisterSID: function(offset) {
			return this.Module.ccall('getRegisterSID', 'number', ['number'], [offset]);
		}
	});	return $this; })();
	