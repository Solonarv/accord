#include <QCommandLineParser>

#ifndef COMMANDLINEOPTIONS_H
#define COMMANDLINEOPTIONS_H


struct CommandLineOptions
{
    enum ApplicationMode {
        PROTOCOL_HANDLER,
        DEFAULT
    };
    const ApplicationMode mode;
    const QStringList positionalArgs;

    CommandLineOptions(const QCommandLineParser &parser);

    static QCommandLineParser* parser();
};



#endif // COMMANDLINEOPTIONS_H
