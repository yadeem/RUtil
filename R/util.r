#'@export
get.func.signature = function(func) {
    if (!is.function(func)) return(NA)

    str = paste0(deparse(func), collapse = "")
    sig = gsub("\\{.*", "", str)
    return(sig)
}

#' nodoc
#' @export
not.implemented = function() {
    fatal.log("not.implemented", "Not implemented", stack.trace.nskip = 2)
}

#' nodoc
#' @export
get.first.ok = function(...) {
    for (x in list(...))
        if (length(x) > 0 && any(!is.na(x))) return(x)
    return(NULL)
}

#' nodoc
#' @export
get.not.null.or.add = function(a, b) {
    if (is.null(a)) return(b)
    if (is.null(b)) return(a)
    return(a + b)
}

#' nodoc
#' @export
gnnadd = get.not.null.or.add

#' nodoc
#' @export
fail.over = function(a, b, fail.func = is.na) {
    if (is.null(a)) return(b)

    is.failed = fail.func(a)
    if (length(b) == 1) {
        a[is.failed] = b
    } else {
        a[is.failed] = b[is.failed]
    }
    return(a)
}

#' nodoc
#' @export
get.mode = function(x, na.rm = TRUE) {
    x = x[!is.na(x)]
    unique.x = unique(x)
    mode = unique.x[which.max(tabulate(match(x, unique.x)))]
    return(mode)
}

#' nodoc
#' @export
copy.attr = function(src, dest, attributes) {
    for (attr in attributes) {
        setattr(dest, attr, attr(src, attr))
    }
}

#' nodoc
#' @export
set.global.var = function(x) {
    assign(deparse(substitute(x)), x, envir = .GlobalEnv)
}

#' nodoc
#' @export
data.table.2.str = function(data.table) {
    if (is.empty(data.table)) return(NULL)

    fdt = format(data.table)

    for (n in 1:ncol(fdt)) {
        str.length = max(str_length(fdt[, n]), str_length(colnames(fdt)[n]))
        colnames(fdt)[n] = str_pad(colnames(fdt)[n], str.length, side = "left")
        fdt[, n] = str_pad(fdt[, n], str.length, side = "left")
    }

    col.names.line = paste0(paste0(colnames(fdt), collapse = " "), "\n")
    ret = col.names.line
    for (n in 1:nrow(fdt)) {
        str.line = paste0(paste0(fdt[n,], collapse = " "), "\n")
        ret = paste0(ret, str.line)
    }
    ret = paste0(ret, col.names.line)

    return(ret)
}

#' nodoc
#' @export
recover.attr = function(data, origin.attr) {

    new.attr = attributes(data)
    ori.attr.name = setdiff(names(origin.attr), names(new.attr))

    if (is.data.table(data)) {
        ori.attr.name = ori.attr.name[ori.attr.name != "index"] # remove index attribute if exists
    }
    ori.attr = origin.attr[ori.attr.name]
    setattributes(data, c(new.attr, ori.attr))
}

#' nodoc
#' @export
recover.attr.from.data = function(data, data.with.attributes) {
    recover.attr(data, attributes(data.with.attributes))
}

#' nodoc
#' @export
move.element = function(vector, element.to.move, element.after, ignore.duplicate = FALSE) {
    index.to.move = which(vector == element.to.move)
    index.after = which(vector == element.after)
    if (length(index.to.move) > 1) {
        if (ignore.duplicate) return(vector)
        fatal.log("move.element", "Multiple element matches the element to move {element.to.move}: {index.to.move}")
    }
    if (length(index.after) > 1) {
        if (ignore.duplicate) return(vector)
        fatal.log("move.element", "Multiple element matches the element after {element.after}: {index.after}")
    }

    len = length(vector)
    if (index.to.move < index.after) {
        # move x to after y
        # 1 ~ x-1,   x+1 ~ y,   x,   y+1 ~ len
        #     ^^^                    ^^^
        #     x could be 1           y could be len
        index = c((index.to.move + 1):index.after, index.to.move)
        if (index.to.move > 1) index = c(1:(index.to.move - 1), index)
        if (index.after < len) index = c(index, (index.after + 1):len)
    } else {
        # move y to after x
        # 1 ~ x,   y,   x+1 ~ y-1,   y+1 ~ len
        #                            ^^^
        #                            y could be len
        index = c(1:index.after, index.to.move, (index.after + 1):(index.to.move - 1))
        if (index.to.move < len) index = c(index, (index.to.move + 1):length(vector))
    }
    return(vector[index])
}

#' set the number of threads used by data.table and fst package, the maximum
#' number of threads that data.table can use is 64(unknown reason), this function
#' will set the default number of data.table and fst to 50% of logical CPUs
#'
#' @export
set.preferred.concurrency = function(n.max.threads = NULL) {

    logical.threads = parallel::detectCores(logical = TRUE)
    if(is.null(n.max.threads)) {
       actual.threads = as.integer(0.5 * logical.threads)
    } else if( n.max.threads > 1) {
       actual.threads = min(as.integer(n.max.threads), logical.threads)
    } else {
       actual.threads = as.integer(n.max.threads * logical.threads)
    }

    actual.threads = max(1, actual.threads)

    if (requireNamespace("data.table", quietly = TRUE)) {
        max.dt.threads = 64
        actual.threads = min(actual.threads, max.dt.threads)
        data.table::setDTthreads(threads = actual.threads)
        info.log("set.preferred.concurrency", "Set the number of threads used by data.table to {actual.threads}")
    } else {
        info.log("set.preferred.concurrency", "Operation is ignored because the data.table package is not installed")
    }

    if (requireNamespace("fst", quietly = TRUE)) {
        fst::threads_fst(actual.threads)
        info.log("set.preferred.concurrency", "Set the number of threads used by fst to {actual.threads}")
    } else {
        info.log("set.preferred.concurrency", "Operation is ignored because the fst package is not installed")
    }

    .preferred.concurrency <<- actual.threads
}

#' nodoc
#' @export
get.preferred.concurrency = function() {
    if(exists(".preferred.concurrency", mode = "numeric")) {
        return(.preferred.concurrency)
    } else {
        warning.log("get.preferred.concurrency", "preferred concurrency is not setted, please call set.preferred.concurrency first")
        return(1)
    }
}

#' nodoc
#' @export
get.running.script.path = function() {
    args = commandArgs(trailingOnly = FALSE)
    pattern = "^--file="
    file.arg = args[grepl(pattern, args)]
    if (length(file.arg) == 0) return(NULL)

    relative.path = sub(pattern, "", file.arg)
    abs.path = file.path(getwd(), relative.path)
    return(abs.path)
}

#' nodoc
#' @export
'%?%' = function(b, x) list(b = b, x = x)

#' nodoc
#' @export
'%:%' = function(bx, y) if (length(bx$b) == 0 || !bx$b) y else bx$x


#' nodoc
#' @export
get.isolated.index.by.boxplot = function(value, coef = 3) {
    stats = boxplot.stats(value, coef)
    out.idx = which(value %in% stats$out)
    return(out.idx)
}

#' nodoc
#' @export
get.isolated.index.by.local.mean = function(value, check.index = NULL, mean.range = 120) {
    delta = abs(max(value) - min(value)) * 0.15
    max.idx = length(value)
    isolate.idx = c()
    if (is.empty(check.index)) check.index = 1:max.idx
    for (i in check.index) {
        around.idx = max(1, (i - mean.range)):min((i + mean.range), max.idx)
        m = mean(value[around.idx], na.rm = T)
        if (abs(value[i] - m) >= delta)
            isolate.idx = c(isolate.idx, i)
    }

    return(isolate.idx)
}

#' nodoc
#' @export
fix.isolated.value = function(value, isolated.idx) {
    if (length(isolated.idx) > 0) {
        min.v = min(value[-isolated.idx])
        max.v = max(value[-isolated.idx])
        fixpx = ifelse(value[isolated.idx] < min.v, min.v, value[isolated.idx])
        fixpx = ifelse(fixpx > max.v, max.v, fixpx)
        value[isolated.idx] = fixpx
    }
    return(value)
}

#' nodoc
#' @export
seperate.index.to.continuous.group = function(index) {
    if (length(index) == 0) return(NULL)
    discontinous.idx = which(diff(index) != 1) + 1
    group.start = index[c(1, discontinous.idx)]
    group.end = index[c(discontinous.idx - 1, length(index))]
    continuous.group = lapply(seq_along(group.start), function(i) group.start[i]:group.end[i])
    return(continuous.group)
}

#' nodoc
#' @export
modify.data.table = function(dt, value) {
    dt.name = colnames(dt)
    v.name = names(value)
    v.name = v.name[nzchar(v.name)]
    for (name in v.name) {
        set(dt, NULL, name, value[[name]])
    }
    return(dt)
}

#' nodoc
#' @export
modify.list = function(x, val, keep.null = FALSE) {
    stopifnot(is.list(x), is.list(val))

    if (is.null(names(x)) && is.null(names(val))) return(val)

    xnames <- names(x)
    vnames <- names(val)
    vnames <- vnames[nzchar(vnames)]
    if (keep.null) {
        for (v in vnames) {
            x[v] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
                list(modify.list(x[[v]], val[[v]], keep.null = keep.null))
            else val[v]
        }
    }
    else {
        for (v in vnames) {
            x[[v]] <- if (v %in% xnames && is.list(x[[v]]) &&
                is.list(val[[v]]))
                modify.list(x[[v]], val[[v]], keep.null = keep.null)
            else val[[v]]
        }
    }
    return(x)
}

#' nodoc
#' @export
modify.data.table.by.var.args = function(dt, ...) {
    var.args = list(...)
    for (name in names(var.args)) {
        value = var.args[[name]]
        if (!is.vector(value)) value = rep_len(list(value), nrow(dt))
        set(dt, NULL, name, value)
    }
    return(dt)
}

#' nodoc
#' @export
confirm = function(confirm.str) {
    info.log("confirm", "{confirm.str} (y/n): ")
    if (interactive())
        answer = readline()
    else
        answer = readLines("stdin", n = 1)

    return(tolower(answer) %in% c("y", "yes"))
}

# insert the insert.data into the base.data before the row indicated by insert.pos
#' @param base.data
#' @param insert.data
#' @param insert.pos a vector, the size could be 1 or equals to the rows of
#' insert.data, and the position value range is [1, nrow(base.data) + 1]
#' the position 1 means insert at the begin of base.data
#' the position nrow(base.data) + 1 means insert at the end of base.data
#' @export
insert.data.table = function(base.data, insert.data, insert.pos) {
    if (is.empty(insert.data)) return(base.data)
    if (is.empty(base.data)) return(insert.data)

    assert(nrow(insert.data) == length(insert.pos) || length(insert.pos) == 1)
    assert(!is.unsorted(insert.pos))
    assert(all(insert.pos <= nrow(base.data) + 1))

    ori.attr = attributes(base.data)
    u.insert.pos = unique(insert.pos)
    new.dt = data.table()
    start = 0
    for (i in u.insert.pos) {
        insert.dt = insert.data[insert.pos == i,]
        new.dt = rbind(new.dt, base.data[start:(i - 1),], insert.dt)
        start = i
    }

    #  add the remain base.data when no data insert at the end of base.data
    if (all(u.insert.pos != nrow(base.data) + 1)) {
        new.dt = rbind(new.dt, base.data[start:nrow(base.data),])
    }

    recover.attr(new.dt, ori.attr)
    return(new.dt)
}

#' nodoc
#' @export
add.dt.row = function(dt, ...) {
    x = data.table(...)
    names(x) = names(dt)
    new.dt = rbind(dt, x)
    recover.attr.from.data(new.dt, dt)
    return(new.dt)
}

#' nodoc
#' @export
percent.to.numeric = function(percent.str) {
    as.numeric(sub("%", "",  percent.str)) * 0.01
}

#' nodoc
#' @export
compare.files = function(a.file, b.file, verbose = TRUE, mode = TRUE) {
    verb.arg = ifelse(verbose, "", "--no-patch")
    mode.arg = ifelse(mode, "", "-G.")
    argument = paste("diff", "--no-index", "--exit-code", verb.arg, mode.arg, a.file, b.file)
    return(bashexec(glue("git {argument}")))
}

#' nodoc
#' @export
set.tz = function(data, tz) {
    setattr(data, "tzone", tz)
}

#' nodoc
#' @export
get.tz = function(data) {
    attr(data, "tzone")
}

#' nodoc
#' @export
splitt.comma.value = function(v, trim = TRUE) {
    ret = unlist(str_split(v, ","))
    return(if(trim) str_trim(ret) else ret)
}