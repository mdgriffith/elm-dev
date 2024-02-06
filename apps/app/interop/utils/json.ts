export const parse = (source: string) => {
  try {
    return JSON.parse(source);
  } catch (err) {
    console.log("Error parsing watchtower Msg:");
    console.log("    " + err);
    console.log("Original Msg");
    console.log(source);
    return null;
  }
};
