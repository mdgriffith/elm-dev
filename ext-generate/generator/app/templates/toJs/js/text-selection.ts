export function focus_and_select(id: string) {
  setTimeout(() => {
    // in some cases the element hasn't been rendered yet
    const elem = document.getElementById(id);
    if (elem) {
      elem.focus();
      (elem as HTMLInputElement).select();
    }
  }, 100);
}
