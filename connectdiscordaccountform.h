#ifndef CONNECTDISCORDACCOUNTFORM_H
#define CONNECTDISCORDACCOUNTFORM_H

#include <QWidget>

namespace Ui {
class ConnectDiscordAccountForm;
}

class ConnectDiscordAccountForm : public QWidget
{
    Q_OBJECT

public:
    explicit ConnectDiscordAccountForm(QWidget *parent = 0);
    ~ConnectDiscordAccountForm();

private:
    Ui::ConnectDiscordAccountForm *ui;
};

#endif // CONNECTDISCORDACCOUNTFORM_H
