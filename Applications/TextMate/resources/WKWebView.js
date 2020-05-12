window.addEventListener("error", function(event) {
	webkit.messageHandlers.textmate.postMessage({
		command: "log",
		payload: {
			level:    "error",
			message:  event.message,
			filename: event.filename,
			lineno:   event.lineno,
			colno:    event.colno
		}
	});
});

let TextMate = {
	log(str) {
		webkit.messageHandlers.textmate.postMessage({
			command: "log",
			payload: {
				message: str
			}
		});
	},
	addLicense(callback) {
		let options = { };
		if(callback !== undefined) {
			this.callbacks.set(this.nextCallbackId, callback);
			options.callbackId = this.nextCallbackId;
			this.nextCallbackId += 1;
		}
		webkit.messageHandlers.textmate.postMessage({
			command: "addLicense",
			payload: options
		});
	},
	addLicenseCallback(callbackId, owner) {
		const callback = this.callbacks.get(callbackId);
		if(callback) {
			callback(owner);
			this.callbacks.delete(callbackId);
		}
		else {
			this.log("No callback supplied for addLicense()");
		}
	},
	nextCallbackId: 1,
	callbacks: new Map(),
	version:	  null,
	licensees: null,
	copyright: null
};
