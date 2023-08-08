#' nodoc
#' @export
new.test.case = function(test.case.name, package.name, function.name, args, expected.output) {
    test.case = list(name = test.case.name,
                     package.name = package.name,
                     function.name = function.name,
                     args = args,
                     expected.output = expected.output,
                     class.name = class.name,
                     class.args = class.args)

    class(test.case) = c(class(test.case), "test.case")
    return(test.case)
}

#' nodoc
#' @export
new.class.test.case = function(test.case.name, package.name, clz.name, init.args, function.name, args, expected.output) {
    test.case = list(name = test.case.name,
                     package.name = package.name,
                     function.name = function.name,
                     args = args,
                     expected.output = expected.output,
                     clz.name = clz.name,
                     init.args = init.args)

    class(test.case) = c(class(test.case), "class.test.case")

    return(test.case)
}

#' nodoc
#' @export
run.test.case = function(test.case) {
    b = suppressMessages(require(test.case$package.name, character.only = TRUE))
    if (!b) fatal.log("run.test.case", "{test.case$name} Cannot load package: {test.case$package.name}")

    if (is(test.case, "class.test.case")) {
        clz.object = do.call(new, c(list(Class = test.case$clz.name), test.case$init.args))
        func = do.call("$", list(clz.object, test.case$function.name))
    } else {
        func = get(test.case$function.name)
    }

    output = do.call(func, test.case$args)
    success = identical(output, test.case$expected.output)
    return(success)
}

#' nodoc
#' @export
run.test.suite = function(path) {
    test.case.files = list.files(path, full.names = TRUE, recursive = TRUE)
    for (f in test.case.files) {
        test.case = get(load(f))
        test.case = update.loaded.data.table(test.case)
        ret = try_capture_stack({
            run.test.case(test.case)
        }, environment())

        if (is(ret, "simpleError")) {
            info.log("run.test.suite", "{test.case$name}: Failed. Path={f} Exception={ret$message} Stacktrace:")
            info.log("run.test.suite", paste0(rev(ret$calls), collapse = "\n"))

        } else if (ret == TRUE) {
            info.log("run.test.suite", "{test.case$name}: Succeed.")

        } else {
            info.log("run.test.suite", "{test.case$name}: Failed. Path={f} Ret={ret}")
        }
    }
}

update.loaded.data.table = function(object) {
    if (is.data.table(object)) {
        old.class = class(object)
        setDT(object)
        class(object) = old.class
    } else if (is.list(object)) {
        for (i in seq_along(object)) {
            object[[i]] = update.loaded.data.table(object[[i]])
        }
    }
    return(object)
}