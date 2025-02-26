const REGEX_COMMENTS = /(^\s+)?-{2,3}.+/gm;

export function minify(code: string): string {
  // Shorten the astropacks statements
  code = code.replace(/astropacks(-|_)aostandard(-|_)/g, "ap_");

  // Stub unused lines
  code = code.replace(/astropacks(-|_)aostandard/g, "__ap__");

  // Remove comments
  code = code.replace(REGEX_COMMENTS, "\n");
  code = code.replace(/\n\s+?--$/gm, "\n");

  // Remove empty lines
  code = code.replace(/^\s*\n/gm, "");

  return code;
}
