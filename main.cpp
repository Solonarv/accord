#include "commandlineoptions.h"
#include "protocolhandler.h"
#include "mainwindow.h"
#include "connectdiscordaccountform.h"
#include <QApplication>
#include <QCommandLineParser>

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    QApplication::setApplicationName("Accord");
    QApplication::setApplicationVersion("0.1.0");


    QCommandLineParser* parser = CommandLineOptions::parser();
    parser->process(app);
    CommandLineOptions options(*parser);

    int fail = 0;
    MainWindow mainWindow;
    switch (options.mode)
    {
    case CommandLineOptions::PROTOCOL_HANDLER:
        for (auto it = options.positionalArgs.constBegin();
             !fail && it != options.positionalArgs.constEnd();
             ++it)
        {
            ProtocolHandler handler(*it);
            fail = handler.process();
            if (fail)
                break;
        }
        return fail;
    case CommandLineOptions::DEFAULT:
        mainWindow.show();

        return app.exec();
    default:
        return -1;
    }
}
