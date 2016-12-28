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

    function request(url, list) {
        return A2(
            _elm_lang$core$Native_Scheduler.andThen,
            function (formData) {
                return _elm_lang$core$Native_Scheduler.nativeBinding(function (callback) {
                    callback(
                        _elm_lang$core$Native_Scheduler.succeed(
                            _elm_lang$http$Http_Internal$Request(
                                A7(
                                    _elm_lang$http$Http_Internal$RawRequest,
                                    'POST',
                                    _elm_lang$core$Native_List.fromArray([]),
                                    url,
                                    {
                                        ctor: 'FormDataBody',
                                        _0: formData
                                    },
                                    _elm_lang$http$Http$expectString,
                                    _elm_lang$core$Maybe$Nothing,
                                    false
                                )
                            )
                        )
                    )
                });
            },
            requestHelper(list, new FormData)
        );
    }

    function requestHelper(list, formData) {
        if (list.ctor == '::') {
            return A2(
                _elm_lang$core$Native_Scheduler.andThen,
                function (formData) {
                    return _elm_lang$core$Native_Scheduler.nativeBinding(function (callback) {
                        var file = list._0;
                        var fileReader = new FileReader();
                        fileReader.addEventListener('load', function (event) {
                            formData.append('file', new File([event.target.result], file.object.name, {type: file.object.type}));
                            callback(_elm_lang$core$Native_Scheduler.succeed(formData));
                        });
                        fileReader.addEventListener('abort', function (event) {
                            callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'FileReadError' }))
                        });
                        fileReader.addEventListener('error', function (event) {
                            callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'FileReadError' }))
                        });
                        fileReader.readAsArrayBuffer(file.object);
                    });
                },
                requestHelper(list._1, formData)
            );
        } else {
            return _elm_lang$core$Native_Scheduler.succeed(formData);
        }
    }

    return {
        decodeFileList: decodeFileList,
        request: F2(request)
    };
}();
