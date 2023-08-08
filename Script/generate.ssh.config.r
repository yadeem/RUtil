suppressMessages(library(RUtil))
suppressMessages(library(data.table))

options(warn = 1, showWarnCalls = TRUE)

args = process.cmd.args(NULL, list(
    file = "SSHConfig.xml",
    output = "SSHConfig",
    deploy = "TRUE",
    client = NULL,
    server = NULL,
    host.ssh.config.path = "~/HostSSHConfig/"
))
if (is.null(args)) quit()

config.file = args$file
output.path = args$output
deploy = as.logical(args$deploy)
client = unlist(str_split(args$client, ","))
server = unlist(str_split(args$server, ","))
dir.create(output.path, FALSE, TRUE)
host.ssh.config.path = args$host.ssh.config.path

ssh.xml.config.data = parse.ssh.config.file(config.file)

client = expand.host(client, ssh.xml.config.data$host.group.config)
server = expand.host(server, ssh.xml.config.data$host.group.config)

ssh.config.data = generate.ssh.config.data(ssh.xml.config.data)
ssh.config.info = generate.ssh.config.file(ssh.config.data, output.path)

if (deploy) deploy.ssh.config.data(ssh.config.info, host.ssh.config.path)
test.ssh.config(ssh.config.data, client, server)