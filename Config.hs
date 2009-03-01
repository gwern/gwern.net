Config {
repository          = Darcs "./",
defaultPageType     = Markdown,
userFile            = "static/gitit-users",
templateFile        = "static/template.html",
staticDir           = "static",
pluginModules       = ["data/InterwikiPlugin.hs"],
tableOfContents     = True,
maxUploadSize       = 10000000,
portNumber          = 5001,
debugMode           = True,
frontPage           = "Front Page",
noEdit              = [],
noDelete            = [],
accessQuestion      = Just ("Enter the access code (to request an access code, contact gwern0@gmail.com):", ["lambda"]),
useRecaptcha        = False,
recaptchaPublicKey  = "",
recaptchaPrivateKey = "",
maxCacheSize        = 6291456,
mimeTypesFile       = "/etc/mime.types"
}

