package facade.Facade;

import facade.system.GetInvoice;
import facade.system.Payment;
import facade.system.ProductDAO;
import facade.system.Send_Notification;

public class OrderFacade {

    ProductDAO productDAO;
    Payment payment;
    GetInvoice getInvoice;
    Send_Notification send_Notification;

    public OrderFacade() {
        this.productDAO = new ProductDAO();
        this.payment = new Payment();
        this.getInvoice = new GetInvoice();
        this.send_Notification = new Send_Notification();
    }

    public void createOrder() {
        productDAO.selectProduct();
        payment.makePayment();
        getInvoice.getInvoice();
        send_Notification.sendNotification();
    }

}
