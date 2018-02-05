#include <QUrl>

#ifndef PROTOCOLHANDLER_H
#define PROTOCOLHANDLER_H

const QString accordScheme = "accord";

class ProtocolHandler
{
public:
    ProtocolHandler(const QString &url);

    int process();
private:
    const QUrl url;
};

#endif // PROTOCOLHANDLER_H
