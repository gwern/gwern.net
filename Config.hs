repository-type: Darcs
# specifies the type of repository used for wiki content.
# Options are Git and Darcs.

repository-path: ./
# specifies the path of the repository directory.  If it does not
# exist, gitit will create it on startup.

user-file: static/gitit-users
# specifies the path of the file containing user login information.
# If it does not exist, gitit will create it (with an empty user list).

template-file: static/template.html
# specifies the path of the page template file.  If it does not exist,
# gitit will create a default template.  Users may wish to edit this
# file to customize the appearance of their wiki.  The template file
# is an HStringTemplate template.  Variables to be interpolated appear
# between $'s. Literal $'s must be backslash-escaped.

log-level: DEBUG
# determines how much information is logged.
# Possible values (from most to least verbose) are DEBUG, INFO,
# NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY.

plugins: data/InterwikiPlugin.hs
# specifies a list of plugins to load.  Plugins may be specified
# either by their path or by their module name.  If the plugin name
# starts with Gitit.Plugin., gitit will assume that the plugin is
# an installed module and will not try to find a source file.
# Examples:
# plugins: plugins/DotPlugin.hs, CapitalizeEmphasisPlugin.hs
# plugins: plugins/DotPlugin
# plugins: Gitit.Plugin.InterwikiLinks

max-cache-size: 10M
# specifies an upper limit on the size of the in-memory page cache,
# in bytes.  (The abbreviations K, M, and G may be used for
# thousand, million, and billion.)

max-upload-size: 10M
# specifies an upper limit on the size (in bytes) of files uploaded
# through the wiki's web interface.

debug-mode: yes
# if "yes", causes debug information to be logged while gitit is running.

access-question: What is the capital of Assyria?
access-question-answers: Nineveh
# specifies a question that users must answer when they attempt to create
# an account, along with a comma-separated list of acceptable answers.
# This can be used to institute a rudimentary password for signing up as
# a user on the wiki, or as an alternative to reCAPTCHA.
# Example:
# access-question:  What is the code given to you by Ms. X?
# access-question-answers:  RED DOG, red dog

