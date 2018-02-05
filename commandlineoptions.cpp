#include "commandlineoptions.h"
#include "constants.h"
#include <QCommandLineParser>

CommandLineOptions::CommandLineOptions(const QCommandLineParser &parser) :
    mode(parser.isSet("urlhandler") ? PROTOCOL_HANDLER : DEFAULT),
    positionalArgs(parser.positionalArguments())
{
}

QCommandLineParser *CommandLineOptions::parser()
{
    auto parser = new QCommandLineParser;
    parser->setApplicationDescription("Accord - a lightweight alternative Discord client");
    parser->addHelpOption();
    parser->addVersionOption();

    QCommandLineOption tokenFile({"f", "tokenfile"},
                                 "Read OAuth2 token from <file>.",
                                 "file",
                                 DEFAULT_FILE(token.json));
    QCommandLineOption token({"t", "token"},
                             "Use <token> as OAuth2 token.",
                             "token");
    QCommandLineOption urlHandler("urlhandler",
                                  "Invoke Accord as protocol handler");

    parser->addOptions({tokenFile, token, urlHandler});

    return parser;
}
