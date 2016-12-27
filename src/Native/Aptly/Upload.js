var _SHyx0rmZ$aptly_web$Native_Aptly_Upload = function () {
    function decodeFileList(value) {
        if (!(value instanceof FileList)) {
            return {tag: 'list', type: 'a FileList', value: value};
        }

        var files = [];

        for (var i = 0; i < value.length; ++i) {
            var file = value.item(i);

            files.push({
                ctor: 'File',
                name: file.name,
                size: file.size,
                mime: file.type,
                object: new File([ file ], file.name)
            });
        }

        return _elm_lang$core$Native_List.fromArray(files);
    }

    function request(url, file) {
        var formData = new FormData();
        formData.append('file', file.object);
        return _elm_lang$http$Http_Internal$Request(
            A7(
                _elm_lang$http$Http_Internal$RawRequest,
                'POST',
                _elm_lang$core$Native_List.fromArray([
                ]),
                url,
                {
                    ctor: 'FormDataBody',
                    _0: formData
                },
                _elm_lang$http$Http$expectString,
                _elm_lang$core$Maybe$Nothing,
                false
            )
        );
    }

    return {
        decodeFileList: decodeFileList,
        request: F2(request)
    };
}();
