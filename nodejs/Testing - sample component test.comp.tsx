import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';
import '@testing-library/jest-dom';
import { act, render, renderHook } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { TolgeeInstance } from '@tolgee/react';
import { ForgotPasswordView, forgotPasswordViewFormSchema } from './forgot-password-view';

const mockResetPasswordForm = renderHook(() =>
    useForm<z.infer<typeof forgotPasswordViewFormSchema>>({
        resolver: zodResolver(forgotPasswordViewFormSchema),
        defaultValues: {
            loginID: '',
            reCAPTCHAResponse: '',
        },
    }),
).result;
const mockTolgee = {
    t: (key) => key,
} as TolgeeInstance;
jest.mock('@tolgee/react', () => ({
    useTolgee: jest.fn().mockImplementation(() => mockTolgee),
})).mock('next-intl/navigation', () => ({
    createSharedPathnamesNavigation: () => ({
        Link: 'Link',
        redirect: jest.fn(),
        usePathname: jest.fn(),
        useRouter: jest.fn(),
    }),
}));

describe('forgot-password-view', () => {
    it('snapshot component', async () => {
        const formSubmitHandler = jest.fn().mockImplementation(() => {
            r.rerender(comp());
            expect(r.baseElement).toMatchSnapshot('form submitting');
        });
        const onClickBackToLogin = jest.fn();
        const user = userEvent.setup();
        const comp = () => (
            <ForgotPasswordView
                form={mockResetPasswordForm.current}
                formSubmitHandler={formSubmitHandler}
                onClickBackToLogin={onClickBackToLogin}
                tolgee={mockTolgee}
            ></ForgotPasswordView>
        );
        const r = render(comp());
        await r.findByText('components.buttons.sendButton');
        expect(r.baseElement).toMatchSnapshot('initial');
        const loginID = r.getByLabelText('components.inputs.loginIDInput');
        await user.click(loginID);
        await user.type(loginID, 'a');
        await user.tab();
        r.rerender(comp());
        expect(r.baseElement).toMatchSnapshot('short username no recaptcha');
        act(() =>
            mockResetPasswordForm.current.setValue('reCAPTCHAResponse', 'rc123456', {
                shouldValidate: true,
                shouldTouch: true,
                shouldDirty: true,
            }),
        );
        r.rerender(comp());
        expect(r.baseElement).toMatchSnapshot('short username with recaptcha');
        act(() => mockResetPasswordForm.current.resetField('loginID'));
        await user.click(loginID);
        await user.type(loginID, 'test@test.com');
        await user.tab();
        r.rerender(comp());
        expect(r.baseElement).toMatchSnapshot('valid values');
        await user.click(r.getByText('components.links.backToLoginLink'));
        expect(onClickBackToLogin).toHaveBeenCalled();
        await user.click(r.getByText('components.buttons.sendButton'));
        expect(formSubmitHandler).toHaveBeenCalledWith(
            {
                loginID: 'test@test.com',
                reCAPTCHAResponse: 'rc123456',
            },
            expect.any(Object),
        );
    });
});
