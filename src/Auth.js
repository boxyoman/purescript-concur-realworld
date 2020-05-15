// module Auth


exports.setItem_ = function (key, value) {
  window.localStorage.setItem(key, value);
};

exports.getItem_ = function (key) {
  return window.localStorage.getItem(key);
};

exports.removeItem_ = function (key) {
  window.localStorage.removeItem(key);
};
