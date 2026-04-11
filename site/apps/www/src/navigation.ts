import { useEffect, useRef, useState } from "react";
import type { AppRoute } from "./routes";
import { routeForPath } from "./routes";

export interface RouteState {
	hash: string;
	pathname: string;
	route: AppRoute;
}

interface ScrollState {
	hash: string;
	pathname: string;
}

const HEADER_OFFSET = 72;

function currentRouteState(): RouteState {
	return {
		hash: window.location.hash,
		pathname: window.location.pathname,
		route: routeForPath(window.location.pathname),
	};
}

function isInternalUrl(url: URL) {
	return url.origin === window.location.origin;
}

function isPlainLeftClick(event: MouseEvent) {
	return (
		!event.defaultPrevented &&
		event.button === 0 &&
		!event.metaKey &&
		!event.ctrlKey &&
		!event.shiftKey &&
		!event.altKey
	);
}

function internalUrlFromTarget(target: EventTarget | null) {
	if (!(target instanceof Element)) {
		return null;
	}

	const anchor = target.closest("a");
	if (!(anchor instanceof HTMLAnchorElement)) {
		return null;
	}
	if (anchor.target && anchor.target !== "_self") {
		return null;
	}
	if (anchor.hasAttribute("download") || !anchor.href) {
		return null;
	}

	const nextUrl = new URL(anchor.href, window.location.href);
	return isInternalUrl(nextUrl) ? nextUrl : null;
}

function scrollToHash(hash: string) {
	const id = hash.startsWith("#") ? hash.slice(1) : hash;
	if (!id) {
		window.scrollTo({ top: 0, left: 0, behavior: "auto" });
		return;
	}

	const element = document.getElementById(id);
	if (element) {
		const top =
			element.getBoundingClientRect().top + window.scrollY - HEADER_OFFSET;
		window.scrollTo({ top, left: 0, behavior: "auto" });
	}
}

function scrollToPageTop() {
	const main = document.querySelector("main");
	if (main instanceof HTMLElement) {
		const top = Math.max(
			main.getBoundingClientRect().top + window.scrollY - 16,
			0,
		);
		window.scrollTo({ top, left: 0, behavior: "auto" });
		return;
	}

	window.scrollTo({ top: 0, left: 0, behavior: "auto" });
}

export function useRouteState(initialRoute: AppRoute) {
	const [state, setState] = useState<RouteState>({
		hash: "",
		pathname: initialRoute.path,
		route: initialRoute,
	});
	const didHydrateRef = useRef(false);
	const previousStateRef = useRef<ScrollState | null>(null);
	const { hash, pathname, route } = state;

	useEffect(() => {
		window.history.scrollRestoration = "manual";
		setState(currentRouteState());

		const handlePopState = () => {
			setState(currentRouteState());
		};

		const handleDocumentClick = (event: MouseEvent) => {
			if (!isPlainLeftClick(event)) {
				return;
			}
			const nextUrl = internalUrlFromTarget(event.target);
			if (!nextUrl) {
				return;
			}

			event.preventDefault();
			window.history.pushState(
				{},
				"",
				`${nextUrl.pathname}${nextUrl.search}${nextUrl.hash}`,
			);
			setState({
				hash: nextUrl.hash,
				pathname: nextUrl.pathname,
				route: routeForPath(nextUrl.pathname),
			});
		};

		window.addEventListener("popstate", handlePopState);
		document.addEventListener("click", handleDocumentClick);
		return () => {
			window.history.scrollRestoration = "auto";
			window.removeEventListener("popstate", handlePopState);
			document.removeEventListener("click", handleDocumentClick);
		};
	}, []);

	useEffect(() => {
		const previous = previousStateRef.current;
		previousStateRef.current = { hash, pathname };

		if (!didHydrateRef.current) {
			didHydrateRef.current = true;
			if (!hash) {
				return;
			}
		}

		window.requestAnimationFrame(() => {
			if (!previous || previous.pathname !== pathname) {
				if (hash) {
					scrollToHash(hash);
					return;
				}

				scrollToPageTop();
				return;
			}

			if (previous.hash !== hash) {
				scrollToHash(hash);
			}
		});
	}, [hash, pathname]);

	return { hash, pathname, route };
}
