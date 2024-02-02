import * as log from "./log";

export const parse = (source: string) => {
  try {
    return JSON.parse(source);
  } catch (err) {
    log.log("Error parsing watchtower Msg:");
    log.log("    " + err);
    log.log("Original Msg");
    log.log(source);
    return null;
  }
};
