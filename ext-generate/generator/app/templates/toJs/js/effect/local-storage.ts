export const connect = (app: any, data: any) => {
  // Listen for messages from Elm
  app.ports?.localStorage?.subscribe?.((message: any) => {
    switch (message.operation) {
      case "save":
        set(message.details.key, message.details.value);
        if (app.ports?.localStorageUpdated) {
          app.ports.localStorageUpdated.send(message.details);
        }
        break;

      case "clear":
        clear(message.details.key);
        break;

      default:
        break;
    }
  });
};


// Actual commands
export const getAll = () => {
  const data: any = {};
  for (var i = 0, len = localStorage.length; i < len; ++i) {
    const key = localStorage.key(i);
    if (key) {
      data[key] = get(key);
    }
  }
  return data;
};

export const get = (key: string): any => {
  const item = localStorage.getItem(key);
  if (item) {
    try {
      return JSON.parse(item);
    } catch (e) {
      return null;
    }
  } else {
    return null;
  }
};

export const set = (key: string, value: any) => {
  localStorage.setItem(key, JSON.stringify(value));
};

export const clear = (key: string) => {
  localStorage.removeItem(key);
};
