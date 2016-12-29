var _SHyx0rmZ$aptly_web$Native_Aptly_Decode = function () {
    function decodeOptionsCollection(value) {
        if (!(value instanceof HTMLCollection)) {
            return {
                tag: 'fail',
                msg: 'Not an HTMLCollection ' + JSON.stringify(value)
            };
        }

        var options = [];

        for (var i = 0; i < value.length; ++i) {
            var option = value.item(i);

            options.push(option.value);
        }

        return _elm_lang$core$Native_List.fromArray(options);
    }

    return {
        decodeOptionsCollection: decodeOptionsCollection
    };
}();
