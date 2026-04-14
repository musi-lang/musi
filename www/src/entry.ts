import "./app.css";

if (import.meta.env.DEV) {
	import("./main");
} else {
	import("./static-main");
}
