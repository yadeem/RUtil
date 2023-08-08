#!/usr/bin/Rscript

suppressMessages(library(RUtil))

options(warn = 1, showWarnCalls = TRUE)

args = process.cmd.args(c("cfg"), list(confirm = TRUE, force= FALSE, filter = NULL))

if (is.null(args)) quit()

deploy(
    args$cfg,
    confirm = as.logical(args$confirm),
    force = as.logical(args$force),
    filter = args$filter)