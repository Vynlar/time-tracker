const simpleInOutbasePath = "SIMPLE_IN_OUT";

export function getToken() {
    return localStorage.getItem(`${simpleInOutbasePath}.token`);
}

export function setToken(token) {
    return localStorage.setItem(`${simpleInOutbasePath}.token`, token);
}