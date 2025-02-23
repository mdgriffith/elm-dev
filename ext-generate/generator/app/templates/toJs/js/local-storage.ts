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
