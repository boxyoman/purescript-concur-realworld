// module Auth


exports.setItem_ = function (key, value) {
  window.localStorage.setItem(key, value);
};

exports.getItem_ = function (key) {
  var result = window.localStorage.getItem(key);
  return result;
};
