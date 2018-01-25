# auth38

I implemented and maintain a yesod website for my employer. For various reasons
we have our own authentication server. I realised fairly early on that it would
make sense to have a Yesod.Auth plugin that communicated with our
authentication server. It took me quite a while and a number of aborted
attempts to get this working. (The number "38" in this project's title was made
up, and may perhaps be a slight exaggeration.)

Documentation for writing a Yesod.Auth plugin is scant. I uploaded this project
in the hope that it would be a useful starting point for others. It's the
`dummy` plugin, extended to the point where login can actually fail, and with
copious annotations.
