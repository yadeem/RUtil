#' nodoc
#' @export
generate.rcpp.project = function(project.name, path, req.empty.dir = TRUE) {
    tmpl.project.path = system.file("RcppProjectTemplate", package = "RUtil")
    tmpl.patten = "RcppPrjTmpl"

    if (req.empty.dir &&
        dir.exists(path) &&
        length(dir(all.files = TRUE))) {
        fatal.log("generate.rcpp.project", "Dest path is not empty while req.empty.dir = TRUE: {path}")
    }

    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    info.log("generate.rcpp.project", "Creating project {project.name} on {path}")

    # copy files
    files = list.files(tmpl.project.path, all.files = T)
    dot.paths = grep("^\\.{1,2}$", files)
    files = files[-dot.paths]
    sapply(file.path(tmpl.project.path, files),
           function(x) file.copy(x, path, recursive = T))

    # get the dest file paths
    files = file.path(path, files)
    files = sapply(files, function(x) {
        if (dir.exists(x))
            list.files(x, all.files = T, full.names = T, recursive = T)
        else
            x
    })
    files = unlist(files)

    # replace file name and contents
    for (f in files) {
        replace.str = project.name
        content = read_file(f)
        new.content = stringr::str_replace_all(content, tmpl.patten, replace.str)
        write_file(new.content, f)
        if (grepl(tmpl.patten, f)) {
            new.file = sub(tmpl.patten, replace.str, f)
            file.rename(f, new.file)
            f = new.file
        }
        info.log("generate.rcpp.project", "Done: {f}")
    }
    info.log("generate.rcpp.project", "Project {project.name} created on {path}")
}