#include "protocolhandler.h"

ProtocolHandler::ProtocolHandler(const QString& url):
    url(url)
{

}

int ProtocolHandler::process()
{
    if (!url.isValid()
            || url.scheme() != accordScheme)
        return 1;

    switch (url.host())
    {
    case "token":
        if (!url.hasQuery())
            return 1;
    default:
        return 1;
    }
}
