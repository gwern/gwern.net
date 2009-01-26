Config {
repository          = Darcs "./",
defaultPageType     = Markdown,
userFile            = "static/gitit-users",
templateFile        = "static/template.html",
staticDir           = "static",
tableOfContents     = True,
maxUploadSize       = 100000,
portNumber          = 5001,
debugMode           = True,
frontPage           = "Front Page",
noEdit              = [],
noDelete            = [],
accessQuestion      = Just ("Enter the access code (to request an access code, contact gwern0@gmail.com):", ["lambda"]),
useRecaptcha        = False,
recaptchaPublicKey  = "",
recaptchaPrivateKey = "",
mimeTypesFile       = "/etc/mime.types"
}

