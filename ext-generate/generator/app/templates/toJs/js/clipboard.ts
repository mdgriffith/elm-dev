export function copy(text: string) {
  const clipboard = navigator.clipboard;
  if (!clipboard) {
    return;
  }
  clipboard.writeText(text);
}
