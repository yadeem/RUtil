#' nodoc
#' @export
parse.ssh.config.file = function(config.file) {
    xml.content = xmlParse(config.file)
    server.config = get.server.config(xml.content)
    host.group.config = get.host.group.config(xml.content, server.config)
    client.config = get.client.config(xml.content, host.group.config)
    access.config = get.access.config(xml.content, host.group.config)
    user.config = get.user.config(xml.content, host.group.config)
    return(list(server.config = server.config,
                client.config = client.config,
                access.config = access.config,
                user.config = user.config,
                host.group.config = host.group.config))
}

xmlGetAttrWithCheck = function(node, name, default = NULL, converter = NULL,
    namespaceDefinition = character(), addNamespace = length(grep(":", name)) > 0) {

    attr = xmlGetAttr(node, name, default, converter, namespaceDefinition, addNamespace)
    if(is.null(attr)) {
        print(node)
        fatal.log("xmlGetAttrWithCheck", "Fail to get attr '{name}' in node above")
    }
    return(attr)
}

get.server.config = function(xml.content) {
    servers.node = getNodeSet(xml.content, "//Servers")
    xml.servers = get.xml.child(servers.node[[1]], "Server")
    server.config = list()
    for (xml.server in xml.servers) {
        server.name = xmlGetAttrWithCheck(xml.server, "name")
        server.info = get.server.node.info(xml.server)
        server.config[[server.name]] = server.info
    }
    return(server.config)
}

get.server.node.info = function(xml.server) {

    server.node = get.server.node.attr(xml.server)

    lk = list()
    for (link.node in get.xml.child(xml.server, "Link")) {
        link.name = xmlGetAttrWithCheck(link.node, "name")
        lk[[link.name]] = get.server.node.attr(link.node)
    }
    if (length(lk) > 0) server.node[["Link"]] = lk

    return(server.node)
}

get.server.node.attr = function(node) {
    node.attr = list()
    node.attr[["ip"]] = xmlGetAttr(node, "ip")
    node.attr[["port"]] = xmlGetAttr(node, "port", converter = as.integer)
    node.attr[["user"]] = xmlGetAttr(node, "user")
    node.attr[["proxy"]] = xmlGetAttr(node, "proxy")
    return(node.attr)
}

get.host.group.config = function(xml.content, server.config) {
    group.node = getNodeSet(xml.content, "//HostGroups")
    if (length(group.node) == 0) return(NULL)

    xml.groups = get.xml.child(group.node[[1]], "HostGroup")

    group.list = list()
    for (xml.group in xml.groups) {
        group.name = xmlGetAttrWithCheck(xml.group, "name")
        hosts = xmlGetAttrWithCheck(xml.group, "hosts")
        hosts = splitt.comma.value(hosts)
        group.list[[group.name]] = hosts
    }

    expanded.group.list = lapply(group.list, function(x)
        parse.host.group(group.list, x))

    # add '*'' group for all server
    expanded.group.list[["*"]] = names(server.config)

    return(expanded.group.list)
}

parse.host.group = function(group.list, group.server, recursive.level = 0) {

    recursive.level = recursive.level + 1
    if (recursive.level >= 10) {
        fatal.log("parse.host.group", "Too many recurisve level for HostGroup")
    }

    all.group.name = names(group.list)
    ret = group.server

    for (server in group.server) {
        if (server %in% all.group.name) {
            expanded.server = parse.host.group(group.list, group.list[[server]], recursive.level)
            ret = c(ret[ret != server], expanded.server)
        }
    }

    return(ret)
}

get.client.config = function(xml.content, host.group.config) {

    client.config = list()

    clients.node = getNodeSet(xml.content, "//Clients")
    if (length(clients.node) == 0) return(NULL)

    xml.client = get.xml.child(clients.node[[1]], "Client")

    for (xml.clt in xml.client) {

        client = xmlGetAttrWithCheck(xml.clt, "name")
        client = splitt.comma.value(client)
        client = expand.host(client, host.group.config)
        assert.not.null(client)

        ssh.version = xmlGetAttr(xml.clt, "sshver")

        for (clt in client) {
            client.config[[clt]] = list(ssh.version = ssh.version)
        }
    }
    return(client.config)
}

get.access.config = function(xml.content, host.group.config) {

    common.access.config = list()
    user.access.config = list()

    accesses.node = getNodeSet(xml.content, "//Accesses")
    if (length(accesses.node) == 0) return(NULL)

    xml.access = get.xml.child(accesses.node[[1]], "Access")

    for (xml.acs in xml.access) {

        user = xmlGetAttr(xml.acs, "user")

        client = xmlGetAttrWithCheck(xml.acs, "client")
        client = splitt.comma.value(client)
        client = expand.host(client, host.group.config)
        assert.not.null(client)

        server = xmlGetAttrWithCheck(xml.acs, "server")
        server = splitt.comma.value(server)
        server = expand.host(server, host.group.config)
        assert.not.null(server)

        link = xmlGetAttr(xml.acs, "link")

        access.grid = expand.grid(client = client, server = server, stringsAsFactors = FALSE)
        for (i in 1:nrow(access.grid)) {
            clt = access.grid$client[i]
            svr = access.grid$server[i]
            if (is.null(user)) {
                common.access.config[[clt]][[svr]] = list(link = link)
            } else {
                user.access.config[[user]][[clt]][[svr]] = list(link = link)
            }
        }
    }
    return(list(common.access.config = common.access.config,
                user.access.config = user.access.config))
}

get.user.config = function(xml.content, host.group.config) {

    user.config = list()
    user.node = getNodeSet(xml.content, "//Users")
    xml.user = get.xml.child(user.node[[1]], "User")

    for (xml.ur in xml.user) {
        user = xmlGetAttrWithCheck(xml.ur, "name")

        client = xmlGetAttrWithCheck(xml.ur, "client")
        client = splitt.comma.value(client)
        client = expand.host(client, host.group.config)

        server = xmlGetAttrWithCheck(xml.ur, "server")
        server = splitt.comma.value(server)
        server = expand.host(server, host.group.config)

        assert.not.null(user)
        assert.not.null(client)
        assert.not.null(server)

        user.grid = expand.grid(client = client, server = server, stringsAsFactors = FALSE)
        user.config[[user]] = rbind(user.config[[user]], user.grid)
    }
    return(user.config)
}

#' nodoc
#' @export
expand.host = function(host, host.group.config) {
    ret = host
    for (h in host) {
        if (h %in% names(host.group.config))
            ret = c(ret[ret != h], host.group.config[[h]])
        }
    return(ret)
}

get.xml.child = function(node, name = NULL) {
    child = xmlChildren(node)
    if (!is.null(name)) child = child[names(child) == name]
    return(child)
}